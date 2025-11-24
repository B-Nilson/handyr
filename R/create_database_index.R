create_database_index <- function(
  db,
  table_name,
  index_cols,
  index_name = NULL
) {
  stopifnot(is.character(db) & length(db) == 1 | is_db_connection(db))
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(is.character(index_cols), length(index_cols) >= 1)
  stopifnot(is.null(index_name) | is.character(index_name) & length(index_name) == 1)

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
