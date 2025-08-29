create_database <- function(
    name,
    type = "sqlite",
    path = "./",
    return_connection = FALSE) {
  dbi_drivers <- list(
    sqlite = c("RSQLite" = \() RSQLite::SQLite()),
    duckdb = c("duckdb" = \() duckdb::duckdb())
  )
  # Handle inputs
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(is.character(path), length(path) == 1)
  stopifnot(type %in% names(dbi_drivers))

  # Check if driver package is installed, prompt to install if not
  rlang::check_installed(names(dbi_drivers[[type]]))

  # Create database directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Create database if it doesn't exist, connect otherwise
  database_file <- stringr::str_detect(name, "\\.") |> 
    ifelse(yes = name, no = paste0(name, ".", type))
  database_path <- paste0(path, "/", database_file)
  db <- dbi_drivers[[type]][[1]]() |>
    DBI::dbConnect(database_path)

  # Return connection or path to db
  if (return_connection) {
    return(db)
  }
  DBI::dbDisconnect(db)
  return(database_path)
}
