create_database_index <- function(
  db,
  table_name,
  index_cols,
  index_name = NULL
) {
  stopifnot(is.character(db) & length(db) == 1 | is_db_connection(db))
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(is.character(index_cols), length(index_cols) >= 1)
  stopifnot(
    is.null(index_name) | is.character(index_name) & length(index_name) == 1
  )

  # Handle db path instead of connection
  if (is.character(db)) {
    db <- connect_to_database(db)
  }

  # Make sql-safe values
  table_name_safe <- table_name |>
    DBI::dbQuoteIdentifier(conn = db)
  table_name_label <- table_name |>
    gsub(pattern = " |\\.", replacement = "_")
  safe_cols <- index_cols |>
    DBI::dbQuoteIdentifier(conn = db) |>
    paste(collapse = ", ")

  # Build index name if not provided
  if (is.null(index_name)) {
    index_name <- index_cols |>
      gsub(pattern = " |\\.", replacement = "_") |>
      paste(collapse = "_")
  }
  index_name <- "%s_%s" |>
    sprintf(table_name_label, index_name) |>
    DBI::dbQuoteIdentifier(conn = db)

  index_query <- "CREATE INDEX IF NOT EXISTS %s ON %s (%s);" |>
    sprintf(index_name, table_name_safe, safe_cols)
  db |>
    DBI::dbExecute(index_query)
}
