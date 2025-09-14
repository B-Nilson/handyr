create_database <- function(
  name,
  type = NULL,
  path = NULL,
  return_connection = TRUE
) {
  # Handle inputs
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.character(type) & length(type) == 1 | is.null(type))
  stopifnot(type %in% .dbi_creatable | is.null(type))
  stopifnot(is.character(path) & length(path) == 1 | is.null(path))
  stopifnot(is.logical(return_connection), length(return_connection) == 1)

  # If no type specified, use file extension - if none assume sqlite
  if (is.null(type)) {
    type <- tools::file_ext(name) |>
      swap("", with = "sqlite")
  }

  # Check if driver package is installed, prompt to install if not
  db_driver <- .dbi_drivers[[type]]
  rlang::check_installed(names(db_driver))

  # Create database directory if it doesn't exist
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }

  # Use name/path/type as needed to build database path
  file_name <- (tools::file_ext(name) == "") |>
    ifelse(
      yes = paste0(name, ".", type),
      no = name
    )
  database_path <- is.null(path) |>
    ifelse(
      yes = file_name,
      no = paste0(path, "/", file_name)
    )
  
  # Create database if it doesn't exist, connect otherwise
  db <- db_driver() |>
    DBI::dbConnect(database_path)

  # Return connection or path to db
  if (return_connection) {
    return(db)
  }
  DBI::dbDisconnect(db)
  return(database_path)
}

.dbi_creatable <- c("sqlite", "duckdb") # not all drivers are creatable by R
.dbi_drivers <- list(
  sqlite = c("RSQLite" = \() RSQLite::SQLite()),
  duckdb = c("duckdb" = \() duckdb::duckdb())
)
