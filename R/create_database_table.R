#' Create table if not already existing
#'
#' Create a table in a database if it doesn't already exist.
#' This is alternative to [DBI::dbCreateTable()] with additional features for creating primary keys, unique indexes, and indexes.
#'
#' @param db A database connection or path to database (if sqlite or duckdb file extension).
#' @param table_name A character string specifying the table to write to.
#' @param new_data A data frame to write to the table.
#' @param primary_keys A character vector of column names to use as the primary key, the main identifier of individual rows in a table.
#'   Multiple columns can be specified and uniqueness will be assessed based on the combination of columns.
#'   (e.g. `primary_keys = c("col1", "col2")` will add a primary key on the combination of `col1` and `col2`).
#' @param unique_indexes A list of character vector(s) of column names to use as unique indexes.
#'   These will be added to the table, in addition to the primary key, and will result in an error if non-unique data is inserted/existing.
#'   Indexes speed up queries by allowing for faster lookups, but can increase the size of the database and reduce write performance.
#' @param indexes A list of character vector(s) of column names to use as indexes.
#' @param insert_data A logical indicating whether to insert the data frame provided in `new_data` into the created table.
#'   If `FALSE`, the table will be created but no data will be inserted.
#' @return A data frame containing the number of rows inserted into the table.
#' @export
create_database_table <- function(
  db,
  table_name,
  new_data,
  primary_keys = NULL,
  unique_indexes = NULL,
  indexes = NULL,
  insert_data = TRUE
) {
  if (is.character(db)) {
    db <- connect_to_database(db)
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

  # Add indexes if provided
  if (length(indexes) > 0) {
    for (i in 1:length(indexes)) {
      index_name <- NULL
      if (!(is.null(names(indexes)) | names(indexes)[i] == "")) {
        index_name <- names(indexes)[i]
      }
      db |>
        create_database_index(
          table_name = table_name,
          index_cols = indexes[[i]],
          index_name = index_name
        )
    }
  }
  invisible(n_rows_inserted)
}

create_database_index <- function(
  db,
  table_name,
  index_cols,
  index_name = NULL
) {
  table_name_safe <- table_name |>
    DBI::dbQuoteIdentifier(conn = db)
  safe_cols <- index_cols |>
    DBI::dbQuoteIdentifier(conn = db)
  if (is.null(index_name)) {
    index_name <- index_cols |>
      gsub(pattern = " |\\.", replacement = "_") |>
      paste(collapse = "_")
  }

  index_query <- "CREATE INDEX IF NOT EXISTS %s_%s ON %s (%s);" |>
    sprintf(
      table_name |> gsub(pattern = " |\\.", replacement = "_"),
      index_name,
      table_name_safe,
      safe_cols |> paste(collapse = ", ")
    )
  db |>
    DBI::dbExecute(index_query)
}
