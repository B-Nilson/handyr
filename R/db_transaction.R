# Run code block in a database transaction (rollback on error)
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