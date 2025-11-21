# Create table if not already existing
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
      if(!(is.null(names(indexes)) | names(indexes)[i] == "")) {
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
