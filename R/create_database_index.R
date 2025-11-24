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
