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