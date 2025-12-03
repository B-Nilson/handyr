#' Create a connection to a database
#'
#' @description
#' Currently supports sqlite, duckdb, and PostgreSQL databases.
#' Must provide connection details for PostgreSQL databases (see [RPostgres::Postgres()]).
#'
#' Be sure to call [DBI::dbDisconnect()] on the database connection when finished working with it.
#'
#' @inheritParams create_database
#' @param ... Additional arguments passed to [DBI::dbConnect()]
#' @export
#' @examples
#' \dontrun{
#'   "example.sqlite" |> create_database(type = "sqlite")
#'   db <- connect_to_database(name = "example.sqlite")
#'   DBI::dbDisconnect(db)
#' }
connect_to_database <- function(
  name,
  type = NULL,
  path = NULL,
  ...
) {
  # If no type specified, use file extension - if none throw error
  file_extension <- tools::file_ext(name)
  if (is.null(type)) {
    type <- file_extension
    if (type == "") {
      stop("No type specified and no file extension found in name.")
    }
  }
  stopifnot("Specified type is not supported." = type %in% names(.dbi_drivers))

  # Check if driver package is installed
  driver <- .dbi_drivers[[type]]
  rlang::check_installed(names(driver))

  # Use name/path/type as needed to build database path if file based
  if (type %in% .dbi_filebased) {
    file_name <- (file_extension == type) |>
      ifelse(
        yes = name,
        no = paste0(name, ".", type)
      )
    database_path <- is.null(path) |>
      ifelse(
        yes = file_name,
        no = paste0(path, "/", file_name)
      )
  }

  # Create connection
  if (type %in% .dbi_filebased) {
    driver[[1]]() |>
      DBI::dbConnect(database_path, ...)
  } else {
    driver[[1]]() |>
      DBI::dbConnect(...)
  }
}
