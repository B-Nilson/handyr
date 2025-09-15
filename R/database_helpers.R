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


db_transaction <- function(db, ...) {
  # Begin a transaction
  DBI::dbBegin(db)
  # run code block passed to function, capture error as warning
  result <- on_error(
    .return = NULL,
    .warn = TRUE,
    ...
  )
  # Rollback the transaction if failed
  if (is.null(result)) {
    DBI::dbRollback(db)
  } else {
    # Commit the transaction, capture error if needed
    result2 <- DBI::dbCommit(db) |>
      on_error(.return = NULL, .warn = TRUE)
    # Rollback the transaction if failed
    if (is.null(result2)) {
      DBI::dbRollback(db)
    }
  }

  return(invisible())
}

is_db_connection <- function(db) {
  on_error(DBI::dbIsValid(db), .return = FALSE)
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