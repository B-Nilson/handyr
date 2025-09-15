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

  # Build sql for determining which entries to delete
  entry_sql <- entry_keys |> 
    as.data.frame() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(dplyr::cur_column(), " = ", .))) |> 
    tidyr::unite("entry_keys", sep = " AND ") |> 
    dplyr::mutate(entry_keys = paste0("(", .data$entry_keys, ")")) |> 
    dplyr::pull("entry_keys") |> 
    paste(collapse = " OR ")
    
  # Build delete query and submit. Return n rows deleted if successful
  delete_query <- "DELETE FROM %s WHERE %s" |>
    sprintf(table_name, entry_sql)
  db |> DBI::dbExecute(delete_query) |> invisible()
}