# Create connection from path to a file-based database
db_conn_from_path <- function(db_path) {
  # Determine backend from file extension
  # TODO: handle variations of extensions
  type <- tools::file_ext(db_path)
  # Handle unsupported backends
  if (!type %in% .dbi_creatable) {
    stop("Could not detect a supported backend for ", db_path, "(based on file extension)")
  }
  # Check if driver package is installed
  rlang::check_installed(names(.dbi_drivers[[type]]))
  # Create connection
  .dbi_drivers[[type]][[1]]() |>
    DBI::dbConnect(db_path)
}

# Run code block in a database transaction (rollback on error)
db_transaction <- function(db, ...) {
  # Begin a transaction
  DBI::dbBegin(db)

  # run code block passed to function, capture error as warning
  transaction_failed <- ... |> 
    on_error(.return = NULL, .warn = TRUE) |> 
    is.null()

  # Rollback the transaction and quit if failed
  if (transaction_failed) {
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

# Check if db is a valid database connection or not
is_db_connection <- function(db) {
  DBI::dbIsValid(db) |> 
    on_error(.return = FALSE)
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