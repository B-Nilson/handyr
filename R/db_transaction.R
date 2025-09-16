#' Submit a transaction to a database, rolling back if an error occurs
#'
#' @param db A database connection or path to database (if sqlite or duckdb file extension).
#' @param ... A code block to run in the transaction. The full request needs to be done here, including connecting to `db`.
#'
#' @return The result of the code block
#' @export
db_transaction <- function(db, ...) {
  # Handle db path instead of connection
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }

  # Begin a transaction
  DBI::dbBegin(db)

  # run code block passed to function, capture error as warning
  result <- ... |> 
    on_error(.return = NULL, .warn = TRUE)

  # Rollback the transaction and quit if failed
  if (is.null(result)) {
    DBI::dbRollback(db)
    stop("Transaction failed.")
  }

  # Commit the transaction, capture error if needed
  result <- DBI::dbCommit(db) |>
    on_error(.return = NULL, .warn = TRUE)

  # Rollback the transaction if failed
  if (is.null(result)) {
    DBI::dbRollback(db)
    stop("Transaction failed.")
  }

  return(invisible(result))
}