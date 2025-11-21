# Create connection from path to a file-based database
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

# Check if db is a valid database connection or not
is_db_connection <- function(db) {
  DBI::dbIsValid(db) |>
    on_error(.return = FALSE)
}

# Convert from R columns types to SQL column types
# TODO: handle more data types
# TODO: are dates handled correctly?
get_sql_column_types <- function(db, new_data, unique_indexes = NULL) {
  # Convert to SQL column types
  sql_types <- db |>
    DBI::dbDataType(new_data)
  # Mark unique columns as not null if specified
  if (!is.null(unique_indexes)) {
    is_unique <- names(new_data) %in% unlist(unique_indexes)
    sql_types[is_unique] <- sql_types[is_unique] |>
      paste("NOT NULL")
  }
  return(sql_types)
}

# Get column names of a database table
db_get_tbl_col_names <- function(db, table_name) {
  dplyr::tbl(db, table_name) |>
    utils::head(1) |>
    dplyr::collect() |>
    colnames()
}
