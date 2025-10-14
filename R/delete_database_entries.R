#' Delete specific entries from a database table.
#'
#' @param db The database connection or path to the database file (if sqlite or duckdb file extension).
#' @param table_name The name of the table to delete entries from.
#' @param entry_keys A data frame or named vector with names matching those in the table and entries equal to the those in the rows to delete.
#' @export
delete_database_entries <- function(db, table_name, entry_keys) {
  stopifnot(is.character(db) & length(db) == 1 | is_db_connection(db))
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(length(entry_keys) >= 1, !is.null(entry_keys))
  rlang::check_installed("DBI")
  rlang::check_installed("dbplyr")

  # Handle db path instead of connection
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }

  # Create temp table
  temp_tbl_name <- paste0("_delete_keys_", table_name)
  db |>
    db_create_table(
      table_name = temp_tbl_name,
      new_data = as.data.frame(entry_keys) |> 
        dplyr::mutate(id = dplyr::row_number()),
      primary_keys = "id"
    )

  # Build delete query using JOIN
  header_match_sql <- "%s.%s = %s.%s" |>
    sprintf(
      temp_tbl_name |> DBI::dbQuoteIdentifier(conn = db),
      names(entry_keys) |> DBI::dbQuoteIdentifier(conn = db),
      table_name |> DBI::dbQuoteIdentifier(conn = db),
      names(entry_keys) |> DBI::dbQuoteIdentifier(conn = db)
    ) |>
    paste(collapse = " AND ")
  delete_query <- "DELETE FROM %s WHERE EXISTS (SELECT 1 FROM %s WHERE %s)" |>
    sprintf(
      DBI::dbQuoteIdentifier(db, table_name),
      DBI::dbQuoteIdentifier(db, temp_tbl_name),
      header_match_sql
    )

  # Execute query
  result <- db |> DBI::dbExecute(delete_query)

  # Cleanup
  db |> DBI::dbRemoveTable(temp_tbl_name)
  invisible(result)
}
