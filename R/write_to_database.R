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
  # TODO: wont work for postgres
  if (is.character(db)) {
    type <- tools::file_ext(db)
    db <- .dbi_drivers[[type]][[1]]() |>
      DBI::dbConnect(db)
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
      db_transaction(db = db)
  } else {
    # Otherwise, merge/insert new data as needed
    db |>
      db_combine_tables(
        new_data = new_data,
        table_name_a = table_name,
        table_name_b = paste0("_", table_name, "_staged"),
        primary_keys = primary_keys,
        unique_indexes = unique_indexes,
        insert_new = insert_new,
        update_duplicates = update_duplicates
      ) |>
      db_transaction(db = db)
  }
  invisible(db)
}

db_create_table <- function(
  db,
  table_name,
  new_data,
  primary_keys,
  unique_indexes = NULL
) {
  if (is.character(db)) {
    type <- tools::file_ext(db)
    db <- .dbi_drivers[[type]][[1]]() |>
      DBI::dbConnect(db)
  }

  # Define SQL templates
  if (is.null(unique_indexes)) {
    create_template <- "CREATE TABLE %s (\n%s,\n%s\n);"
  } else {
    create_template <- "CREATE TABLE %s (\n%s,\n%s,\n%s\n);"
  }
  primary_key_template <- "\tPRIMARY KEY (%s)"
  column_def_template <- '\t"%s" %s'
  unique_constraint_template <- "\tUNIQUE (%s)"

  # Build primary key SQL
  # TODO: quotes dont work for MySQL (backticks) or SQL server (sqr brackets)
  primary_key_sql <- paste0('"', primary_keys, '"') |>
    paste0(collapse = ", ")
  primary_key_sql <- primary_key_template |>
    sprintf(primary_key_sql)

  # Build column definition SQL
  column_types <- new_data |>
    get_sql_column_types(unique_indexes = unique_indexes)
  column_def_sql <- column_def_template |>
    sprintf(names(new_data), column_types) |>
    paste(collapse = ",\n")

  # Build unique constraint SQL
  if (is.null(unique_indexes)) {
    unique_constraint_sql <- ""
  } else {
    unique_constraint_sql <- unique_indexes |>
      sapply(\(unique_ids) {
        unique_ids <- paste0('"', unique_ids, '"') |>
          paste0(collapse = ", ")
        unique_constraint_template |>
          sprintf(unique_ids)
      }) |>
      paste(collapse = ",\n")
  }

  # Build table creation query
  create_query <- create_template |>
    sprintf(
      table_name,
      column_def_sql,
      primary_key_sql,
      unique_constraint_sql
    ) |>
    suppressWarnings() # handle no unique indexes

  # Create table
  db |> DBI::dbExecute(create_query)

  # insert rows if provided
  if (nrow(new_data)) {
    db |> db_insert_rows(new_data = new_data, table_name = table_name)
  }
  invisible(dplyr::tbl(db, table_name))
}

db_insert_rows <- function(db, table_name, new_data) {
  insert_template <- "INSERT INTO %s (%s)\nVALUES\n%s;"

  # Make values SQL
  values_sql <- new_data |>
    dplyr::mutate(
      # Replace NAs with -Inf (swapped with NULL later)
      dplyr::across(dplyr::everything(), ~ swap(., NA, -Inf)),
      # Wrap strings in quotes in case they contain commas/spaces etc
      # TODO: what about strings with quotes in them?
      # TODO: what about dates/times?
      dplyr::across(dplyr::where(is.character), ~ paste0("'", ., "'"))
    ) |>
    tidyr::unite("values", sep = ", ") |>
    # Replace -Inf placeholder with NULL
    dplyr::mutate(
      values = paste0("(", .data$values, ")") |>
        gsub(pattern = "'-Inf'|-Inf", replacement = "NULL")
    ) |>
    # Combine into single string
    dplyr::pull(values) |>
    paste(collapse = ",\n")

  # Build insert query
  insert_query <- insert_template |>
    sprintf(
      table_name,
      paste0('"', names(new_data), '"') |> paste(collapse = ", "),
      values_sql
    )

  # Insert values, return n rows inserted
  db |> DBI::dbExecute(insert_query) |> invisible()
}

db_combine_tables <- function(
  db,
  new_data,
  table_name_a,
  table_name_b,
  primary_keys,
  unique_indexes = NULL,
  insert_new = TRUE,
  update_duplicates = FALSE
) {
  # Create a staging table
  db |>
    db_create_table(
      new_data = new_data,
      table_name = table_name_b,
      primary_keys = primary_keys,
      unique_indexes = unique_indexes
    )
  # Merge overlaps between staged and existing if requested
  if (update_duplicates) {
    db |>
      db_merge_overlap(
        table_name_a = table_name_a,
        table_name_b = table_name_b,
        primary_keys = primary_keys
      )
  }
  # Insert non-overlapping values if requested
  if (insert_new) {
    db |>
      db_insert_new(
        table_name_a = table_name_a,
        table_name_b = table_name_b,
        primary_keys = primary_keys
      )
  }
  # Remove "_staged" table
  db |>
    DBI::dbRemoveTable(table_name_b)
}

get_sql_column_types <- function(new_data, unique_indexes = NULL) {
  column_types <- new_data |>
    sapply(class)
  sql_types <- dplyr::case_when(
    column_types %in%
      c("integer", "logical", "Date", "POSIXct", "POSIXlt", "POSIXt") ~
      "INTEGER",
    column_types == "numeric" ~ "REAL",
    TRUE ~ "TEXT"
  )
  if (!is.null(unique_indexes)) {
    is_unique <- names(new_data) %in% unlist(unique_indexes)
    sql_types[is_unique] <- paste(sql_types[is_unique], "NOT NULL")
  }
  return(sql_types)
}

db_merge_overlap <- function(
    db,
    table_name_a,
    table_name_b,
    primary_keys) {
  # Handle db path instead of connection
  if (is.character(db)) {
    type <- tools::file_ext(db)
    db <- .dbi_drivers[[type]][[1]]() |>
      DBI::dbConnect(db)
  }

  # Builder header renaming sql
  match_header_template <- '\t"%s" = s."%s"'
  col_names <- utils::head(dplyr::tbl(db, table_name_a), 1) |>
    dplyr::collect() |>
    colnames()
  match_header_sql <- match_header_template |>
    sprintf(
      col_names[!col_names %in% primary_keys],
      col_names[!col_names %in% primary_keys]
    ) |>
    paste(collapse = ",\n")

  # Build overlap test sql
  overlap_test_template <- '%s."%s" = s."%s"'
  overlap_test_sql <- overlap_test_template |>
    sprintf(
      rep(table_name_a, length(primary_keys)),
      primary_keys,
      primary_keys
    ) |>
    paste(collapse = " AND ")

  merge_template <- "UPDATE %s\nSET\n%s\nFROM %s s\nWHERE %s;"
  merge_query <- merge_template |>
    sprintf(
      table_name_a,
      match_header_sql,
      table_name_b,
      overlap_test_sql
    )

  db |> DBI::dbExecute(merge_query)
}

db_insert_new <- function(
    db,
    table_name_a,
    table_name_b,
    primary_keys) {
  # Build header insert sql
  col_names <- dplyr::tbl(db, table_name_a) |>
    utils::head(1) |>
    dplyr::collect() |>
    colnames()
  header_insert_sql <- paste0('"', col_names, '"') |>
    paste(collapse = ", ")

  # Build header select sql
  header_select_sql <- paste0('s."', col_names, '"') |>
    paste(collapse = ", ")

  # Build overlap test sql
  overlap_test_template <- 's."%s" = o."%s"'
  overlap_test_sql <- overlap_test_template |>
    sprintf(primary_keys, primary_keys) |>
    paste(collapse = " AND ")

  # Build not overlap test sql
  not_overlap_test_template <- 'o."%s" IS NULL'
  not_overlap_test_sql <- not_overlap_test_template |>
    sprintf(primary_keys) |>
    paste(collapse = " AND ")

  # Build insert query
  insert_template <- "INSERT INTO %s (%s)\nSELECT %s\nFROM %s s LEFT JOIN %s o ON %s\nWHERE %s;"
  sql_query <- insert_template |>
    sprintf(
      table_name_a,
      header_insert_sql,
      header_select_sql,
      table_name_b,
      table_name_a,
      overlap_test_sql,
      not_overlap_test_sql
    )
  db |> DBI::dbExecute(sql_query)
}

db_transaction <- function(db, ...) {
  # Begin a transaction
  DBI::dbBegin(db)
  # run code block passed to function, capture error as warning
  result <- on_error(
    .return = NULL,
    .warn = TRUE,
    ...
  )
  # Rollback the transaction if failed
  if (is.null(result)) {
    DBI::dbRollback(db)
  } else {
    # Commit the transaction, capture error if needed
    result2 <- DBI::dbCommit(db) |>
      on_error(.return = NULL, .warn = TRUE)
    # Rollback the transaction if failed
    if (is.null(result2)) {
      DBI::dbRollback(db)
    }
  }

  return(invisible())
}

is_db_connection <- function(db) {
  on_error(DBI::dbIsValid(db), .return = FALSE)
}
