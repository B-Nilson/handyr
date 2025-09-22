#' Write data to a database
#'
#' Write data to a database, with options to insert new data, update existing data, and create a new table.
#' 
#' It will create the table and insert `new_data` if the table does not already exist.
#' Otherwise:
#'   * If `insert_new = TRUE`, `new_data` will be inserted into the existing table.
#'   * If `update_duplicates = TRUE`, existing data will be updated with the overlap of `new_data`.
#' @param db A database connection or path to database (if sqlite or duckdb file extension).
#' @param table_name A character string specifying the table to write to.
#' @param new_data A data frame to write to the table.
#' @param primary_keys A character vector of column names to use as the primary key, the main identifier of individual rows in a table. 
#'   Multiple columns can be specified and uniqueness will be assessed based on the combination of columns.
#'   (e.g. `primary_keys = c("col1", "col2")` will add a primary key on the combination of `col1` and `col2`).
#' @param unique_indexes A list of character vector(s) of column names to use as unique indexes.
#'   These will be added to the table, in addition to the primary key.
#'   Multiple columns can be specified and uniqueness will be assessed based on the combination of columns.
#'   (e.g. `unique_indexes = list(c("col1", "col2"))` will add a unique index on the combination of `col1` and `col2`.).
#'   If `NULL` (the default), no unique indexes will be added.
#' @param insert_new A logical value indicating if new data should be inserted into the existing table.
#'   If `FALSE`, no new data will be inserted, only existing rows will be updated if `update_duplicates = TRUE`.
#' @param update_duplicates A logical value indicating if existing rows should be updated with new data.
#'   If `FALSE`, no existing rows will be updated. Only new data will be inserted if `insert_new = TRUE`.
#' 
#' @return An invisible db connection.
#' @export
write_to_database <- function(
    db,
    table_name,
    new_data,
    primary_keys,
    unique_indexes = NULL,
    insert_new = TRUE,
    update_duplicates = FALSE) {
  stopifnot(is.character(db) & length(db) == 1 | is_db_connection(db))
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(is.data.frame(new_data))
  stopifnot(is.character(primary_keys), length(primary_keys) >= 1)
  stopifnot(is.list(unique_indexes) | is.null(unique_indexes))
  stopifnot(is.logical(insert_new), length(insert_new) == 1)
  stopifnot(is.logical(update_duplicates), length(update_duplicates) == 1)
  rlang::check_installed("DBI")
  rlang::check_installed("dbplyr")

  # Handle db path instead of connection
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }

  # Create initial table if not already existing
  if (!DBI::dbExistsTable(db, table_name)) {
    db |>
      db_create_table(
        new_data = new_data,
        table_name = table_name,
        primary_keys = primary_keys,
        unique_indexes = unique_indexes
      ) |>
      DBI::dbWithTransaction(conn = db) |>
      on_error(.warn = "Failed to create table.")
  } else {
    # Otherwise, merge/insert new data as needed
    db |>
      db_combine_tables(
        table_name = table_name,
        new_data = new_data,
        primary_keys = primary_keys,
        unique_indexes = unique_indexes,
        insert_new = insert_new,
        update_duplicates = update_duplicates
      ) |>
      DBI::dbWithTransaction(conn = db) |>
      on_error(.warn = "Failed to merge/insert data.")
  }
  invisible(db)
}

# Create table if not already existing
db_create_table <- function(
  db,
  table_name,
  new_data,
  primary_keys,
  unique_indexes = NULL
) {
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }

  # Build primary key SQL
  # TODO: quotes dont work for MySQL (backticks) or SQL server (sqr brackets)
  primary_keys_safe <- paste0('"', primary_keys, '"')
  primary_key_sql <- "\tPRIMARY KEY (%s)" |>
    sprintf(paste0(primary_keys_safe, collapse = ", "))

  # Build column definition SQL
  # TODO: handle more data types
  column_types <- db |>
    get_sql_column_types(new_data = new_data, unique_indexes = unique_indexes)
  column_def_sql <- '\t"%s" %s' |>
    sprintf(names(new_data), column_types) |>
    paste(collapse = ",\n")

  # Build unique constraint SQL
  if (is.null(unique_indexes)) {
    unique_constraint_sql <- ""
  } else {
    unique_constraint_sql <- unique_indexes |>
      sapply(\(unique_ids) {
        unique_ids_safe <- unique_ids |>
          DBI::dbQuoteIdentifier(conn = db) |>
          paste0(collapse = ", ")
        "\tUNIQUE (%s)" |> sprintf(unique_ids_safe)
      }) |>
      paste(collapse = ",\n")
  }

  # Build table creation query
  create_query <- is.null(unique_indexes) |> 
    ifelse(
      "CREATE TABLE %s (\n%s,\n%s\n);",
      "CREATE TABLE %s (\n%s,\n%s,\n%s\n);"
    ) |>
    sprintf(
      table_name |> DBI::dbQuoteIdentifier(conn = db),
      column_def_sql,
      primary_key_sql,
      unique_constraint_sql
    ) |>
    suppressWarnings() # handle no unique indexes

  # Create table
  db |> DBI::dbExecute(create_query)

  # insert rows if provided
  if (nrow(new_data)) {
    n_rows_inserted <- db |>
      DBI::dbAppendRows(value = new_data, name = table_name)
  } else {
    n_rows_inserted <- 0
  }
  invisible(n_rows_inserted)
}

# Merge new and/or overlapping data 
db_combine_tables <- function(
  db,
  table_name,
  new_data,
  primary_keys,
  unique_indexes = NULL,
  insert_new = TRUE,
  update_duplicates = FALSE
) {
  table_name_staging <- paste0("_", table_name, "_staged")
  table_name_staging_safe <- table_name_staging |>
    DBI::dbQuoteIdentifier(conn = db)
  # Create a staging table
  db |>
    db_create_table(
      new_data = new_data,
      table_name = table_name_staging,
      primary_keys = primary_keys,
      unique_indexes = unique_indexes
    )
  # Merge overlaps between staged and existing if requested
  if (update_duplicates) {
    db |>
      db_merge_overlap(
        table_name_a = table_name,
        table_name_b = table_name_staging,
        primary_keys = primary_keys
      )
  }
  # Insert non-overlapping values if requested
  if (insert_new) {
    db |>
      db_merge_new(
        table_name_a = table_name,
        table_name_b = table_name_staging,
        primary_keys = primary_keys
      )
  }
  # Remove "_staged" table
  db |>
    DBI::dbRemoveTable(table_name_staging_safe)
}

# Merge overlapping data from table b into table a based on primary key(s)
db_merge_overlap <- function(
    db,
    table_name_a,
    table_name_b,
    primary_keys) {
  # Handle db path instead of connection
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }

  # Get non-pKey headers
  col_names <- db |>
    db_get_tbl_col_names(table_name = table_name_a)
  col_names <- col_names[!col_names %in% primary_keys]

  # Make sql-safe values
  table_name_a_safe <- table_name_a |> DBI::dbQuoteIdentifier(conn = db)
  table_name_b_safe <- table_name_b |> DBI::dbQuoteIdentifier(conn = db)
  primary_keys_safe <- primary_keys |> DBI::dbQuoteIdentifier(conn = db)
  col_names_safe <- col_names |> DBI::dbQuoteIdentifier(conn = db)

  # Builder header matching sql
  match_header_sql <- '\t%s = _b.%s' |>
    sprintf(col_names_safe, col_names_safe) |>
    paste(collapse = ",\n")

  # Build key overlap test sql
  overlap_test_sql <- '%s.%s = _b.%s' |>
    sprintf(
      table_name_a_safe |> rep(length(primary_keys)),
      primary_keys_safe,
      primary_keys_safe
    ) |>
    paste(collapse = " AND ")

  # Build and execute merge query, return n_rows updated
  merge_query <- "UPDATE %s\nSET\n%s\nFROM %s _b\nWHERE %s;" |>
    sprintf(
      table_name_a_safe,
      match_header_sql,
      table_name_b_safe,
      overlap_test_sql
    )
  db |> DBI::dbExecute(merge_query)
}

# Insert non-overlapping values into table a from table b based on primary keys
db_merge_new <- function(
    db,
    table_name_a,
    table_name_b,
    primary_keys) {
  # Determine columns to insert
  col_names <- db |>
    db_get_tbl_col_names(table_name = table_name_b)

  # Build sql-safe values
  col_names_safe <- col_names |> DBI::dbQuoteIdentifier(conn = db)
  primary_keys_safe <- primary_keys |> DBI::dbQuoteIdentifier(conn = db)
  table_name_a_safe <- table_name_a |> DBI::dbQuoteIdentifier(conn = db)
  table_name_b_safe <- table_name_b |> DBI::dbQuoteIdentifier(conn = db)

  # Build header sql for each table
  a_header_sql <- col_names_safe |> paste(collapse = ", ")
  b_header_sql <- paste0("_b.", col_names_safe) |> paste(collapse = ", ")

  # Build overlap test sql
  overlap_test_sql <- '_b.%s = _a.%s' |>
    sprintf(primary_keys_safe, primary_keys_safe) |>
    paste(collapse = " AND ")

  # Build not overlap test sql (so we can exclude in left join)
  not_overlap_test_sql <- '_a.%s IS NULL' |>
    sprintf(primary_keys_safe) |>
    paste(collapse = " AND ")

  # Build insert query and execute, return n_rows inserted
  sql_query <- "INSERT INTO %s (%s)\nSELECT %s\nFROM %s _b LEFT JOIN %s _a ON %s\nWHERE %s;" |>
    sprintf(
      table_name_a_safe,
      a_header_sql,
      b_header_sql,
      table_name_b_safe,
      table_name_a_safe,
      overlap_test_sql,
      not_overlap_test_sql
    )
  db |> DBI::dbExecute(sql_query)
}
