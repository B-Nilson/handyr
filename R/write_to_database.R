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
#' @param use_on_conflict A logical value indicating if the `ON CONFLICT` clause should be used when updating existing rows.
#'   This will be faster for bulk inserts, but requires a unique contraint on the provided `primary_keys`,
#'   and `db` must support `ON CONFLICT` (e.g. SQLite, Postgres).
#'   If `FALSE`, the `ON CONFLICT` clause will not be used.
#' @param skip_checks
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
  update_duplicates = FALSE,
  use_on_conflict = FALSE,
  skip_checks = FALSE
) {
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
    if (skip_checks) {
      db |>
        DBI::dbWriteTable(
          value = new_data,
          name = table_name,
          append = TRUE,
          row.names = FALSE
        ) |>
        DBI::dbWithTransaction(conn = db) |>
        on_error(.warn = "Failed to append data.")
    } else {
      db |>
        db_combine_tables(
          table_name = table_name,
          new_data = new_data,
          primary_keys = primary_keys,
          unique_indexes = unique_indexes,
          insert_new = insert_new,
          update_duplicates = update_duplicates,
          use_on_conflict = use_on_conflict
        ) |>
        DBI::dbWithTransaction(conn = db) |>
        on_error(.warn = "Failed to merge/insert data.")
    }
  }
  invisible(db)
}

# Create table if not already existing
db_create_table <- function(
  db,
  table_name,
  new_data,
  primary_keys = NULL,
  unique_indexes = NULL,
  insert_data = TRUE
) {
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }
  table_name_safe <- table_name |>
    DBI::dbQuoteIdentifier(conn = db)

  # Build column definition SQL
  column_types <- db |>
    get_sql_column_types(new_data = new_data, unique_indexes = unique_indexes)
  column_def_sql <- '\t"%s" %s' |>
    sprintf(names(new_data), column_types) |>
    paste(collapse = ",\n")

  # Build primary key SQL
  if (is.null(primary_keys)) {
    primary_key_sql <- ""
  } else {
    primary_keys_safe <- primary_keys |> DBI::dbQuoteIdentifier(conn = db)
    primary_key_sql <- "\tPRIMARY KEY (%s)" |>
      sprintf(paste0(primary_keys_safe, collapse = ", "))
  }

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

  # Build table creation query - defer constraints if postgresql for speed
  is_postgres <- inherits(db, "PqConnection") | inherits(db, "PostgreSQL")
  create_query <- "CREATE TABLE %s (\n%s%s%s\n);" |>
    sprintf(
      table_name_safe,
      column_def_sql,
      (is.null(primary_keys) | is_postgres) |>
        ifelse("", paste(",\n", primary_key_sql)),
      (is.null(unique_indexes) | is_postgres) |>
        ifelse("", paste(",\n", unique_constraint_sql))
    )

  # Create table
  db |> DBI::dbExecute(create_query)

  # insert rows if provided
  if (nrow(new_data) & insert_data) {
    success <- db |>
      DBI::dbWriteTable(
        value = new_data,
        name = table_name,
        append = TRUE
      )
    n_rows_inserted <- ifelse(success, nrow(new_data), 0)
  } else {
    n_rows_inserted <- 0
  }

  # Add constraints if defered
  if (is_postgres) {
    pkey_query <- "ALTER TABLE %s ADD %s;" |>
      sprintf(table_name_safe, primary_key_sql)
    db |>
      DBI::dbExecute(pkey_query)
    if (!is.null(unique_indexes)) {
      # TODO: test this
      constraint_query <- "ALTER TABLE %s ADD CONSTRAINT %s;" |>
        sprintf(table_name_safe, unique_constraint_sql)
      db |>
        DBI::dbExecute(constraint_query)
    }
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
  update_duplicates = FALSE,
  use_on_conflict = FALSE
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
  if (insert_new & update_duplicates & use_on_conflict) {
    # Do both insert and merge
    db |>
      db_upsert_from(
        table_name_a = table_name,
        table_name_b = table_name_staging,
        primary_keys = primary_keys
      )
  } else {
    # Merge overlaps between staged and existing if requested
    if (update_duplicates) {
      db |>
        db_update_from(
          table_name_a = table_name,
          table_name_b = table_name_staging,
          primary_keys = primary_keys
        )
    }
    # Insert non-overlapping values if requested
    if (insert_new) {
      db |>
        db_insert_from(
          table_name_a = table_name,
          table_name_b = table_name_staging,
          primary_keys = primary_keys,
          use_on_conflict = use_on_conflict
        )
    }
  }
  # Remove "_staged" table
  db |>
    DBI::dbRemoveTable(table_name_staging_safe)
}

# Merge overlapping data from table b into table a based on primary key(s)
db_update_from <- function(
  db,
  table_name_a,
  table_name_b,
  primary_keys
) {
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
db_insert_from <- function(
  db,
  table_name_a,
  table_name_b,
  primary_keys,
  use_on_conflict = FALSE
) {
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
  if (!use_on_conflict) {
    sql_query <- paste(
      sep = "\n",
      "INSERT INTO %s (%s)",
      "  SELECT %s",
      "  FROM %s _b",
      "  LEFT JOIN %s _a",
      "  ON %s",
      "  WHERE %s;"
    ) |>
      sprintf(
        table_name_a_safe, # insert
        a_header_sql, # insert
        b_header_sql, # select
        table_name_b_safe, # from
        table_name_a_safe, # left join
        overlap_test_sql, # on
        not_overlap_test_sql # where
      )
  } else {
    sql_query <- paste(
      sep = "\n",
      "INSERT INTO %s (%s)",
      "  SELECT %s",
      "  FROM %s",
      "  ON CONFLICT (%s) DO NOTHING;"
    ) |>
      sprintf(
        table_name_a_safe, # insert
        a_header_sql, # insert
        b_header_sql |> stringr::str_remove_all("_b\\."), # select
        table_name_b_safe, # from
        primary_keys_safe |> paste(collapse = ", ") # on
      )
  }
  db |> DBI::dbExecute(sql_query)
}

# Requires ON CONFLICT to work
db_upsert_from <- function(
  db,
  table_name_a,
  table_name_b,
  primary_keys
) {
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

  # Build set sql for updating conflicts
  col_names_safe_no_pk <- col_names_safe[!col_names_safe %in% primary_keys_safe]
  set_sql <- '%s = EXCLUDED.%s' |>
    sprintf(col_names_safe_no_pk, col_names_safe_no_pk) |>
    paste(collapse = ",\n    ")

  # Build insert query and execute, return n_rows inserted
  sql_query <- paste(
    sep = "\n",
    "INSERT INTO %s (%s)",
    "  SELECT %s",
    "  FROM %s",
    "  ON CONFLICT (%s) DO UPDATE",
    "  SET \n    %s;"
  ) |>
    sprintf(
      table_name_a_safe, # insert
      a_header_sql, # insert
      b_header_sql |> stringr::str_remove_all("_b\\."), # select
      table_name_b_safe, # from
      primary_keys_safe |> paste(collapse = ", "), # on
      set_sql # set
    )
  db |> DBI::dbExecute(sql_query)
}
