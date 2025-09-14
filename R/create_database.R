#' Create a file-based database
#'
#' This function creates a new database using either sqlite or duckdb.
#' See [DBI::dbConnect()] for more details.
#'
#' @param name The name of (and optionally path to) the database to create.
#'   If the file extension does not match the type, it will be added.
#' @param type The type of database to create.
#'   If NULL (the default), the type will be derived from `name`, defaulting to "sqlite".
#' @param path The location to save the database file to.
#'   If NULL (the default), the path will be derived from `name`, defaulting to the current working directory.
#' @param version The version of the database to create. Currently only used for `type = "postgresql"`.
#'   Default is `NULL` which uses `"17.0-1"`.
#' @param credentials A vector of length 2 containing the username and password for the database.
#'   Currently only used for `type = "postgresql"`.
#'   Default is `c("USER", "PASSWORD")`.
#' @param port The port to use for the database server. Currently only used for `type = "postgresql"`.
#'   Default is `5432`.
#' @param return_connection A logical value indicating whether to return the database connection instead of the path to the database file.
#'   Default is `TRUE`.
#' @export
create_database <- function(
    name,
    type = NULL,
    path = NULL,
    version = NULL,
    credentials = c("USER", "PASSWORD"),
    port = 5432,
    return_connection = TRUE) {
  # Handle inputs
  stopifnot(is.character(name), length(name) == 1, name != "")
  stopifnot(is.character(type) & length(type) == 1 | is.null(type))
  stopifnot(type %in% .dbi_creatable | is.null(type))
  stopifnot(is.character(path) & length(path) == 1 | is.null(path))
  stopifnot(is.character(version) & length(version) == 1 | is.null(version))
  stopifnot(is.character(credentials), length(credentials) == 2)
  stopifnot(is.numeric(port), length(port) == 1)
  stopifnot(is.logical(return_connection), length(return_connection) == 1)
  rlang::check_installed("DBI")
  rlang::check_installed("dbplyr")

  # If no type specified, use file extension - if none assume sqlite
  if (is.null(type)) {
    type <- tools::file_ext(name) |>
      swap("", with = "sqlite")
  }

  # Check if driver package is installed, prompt to install if not
  db_driver <- .dbi_drivers[[type]]
  rlang::check_installed(names(db_driver))

  # For  postgresql databases ---------------
  # TODO: handle non-windows installs
  # TODO: allow for path to be adjusted?
  # TODO: allow for path to data be returned?
  # TODO: allow for non-local server?
  if (type == "postgresql") {
    name |>
      setup_postgres_db(
        version = version,
        port = port,
        user = credentials[1],
        password = credentials[2]
      )
    db <- name |>
      connect_to_postgres_db(
        version = version,
        port = port,
        user = credentials[1],
        password = credentials[2]
      )
    return(db)
  }

  # For sqlite and duckdb databases --------------
  # Create database directory if it doesn't exist
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }

  # Use name/path/type as needed to build database path
  file_name <- (tools::file_ext(name) == type) |>
    ifelse(
      yes = name,
      no = paste0(name, ".", type)
    )
  database_path <- is.null(path) |>
    ifelse(
      yes = file_name,
      no = paste0(path, "/", file_name)
    )

  # Create database if it doesn't exist, connect otherwise
  db <- db_driver[[1]]() |>
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
  duckdb = c("duckdb" = \() duckdb::duckdb()),
  postgresql = c("RPostgres" = \() RPostgres::Postgres())
)

setup_postgres_db <- function(
    db_name = "mydb",
    version = NULL,
    port = 5432,
    user = "postgres",
    password = "postgres") {
  rlang::check_installed("rappdirs")
  if (is.null(version)) {
    version <- "17.0-1"
  }
  # TODO: if user/pass are NULL, prompt user to enter
  # TODO: handle non-windows installs
  if (!.Platform$OS.type == "windows") {
    stop("This function currently only supports Windows.")
  }
  # Define persistent folders
  base_dir <- rappdirs::user_data_dir("postgres_handyr") |>
    file.path(version)
  bin_dir <- file.path(base_dir, "pgsql", "bin")
  zip_name <- sprintf("postgresql-%s-windows-x64-binaries.zip", version)
  zip_path <- file.path(base_dir, zip_name)
  zip_src <- paste0("https://get.enterprisedb.com/postgresql/", zip_name)
  data_dir <- file.path(base_dir, "data", db_name)
  initdb <- file.path(bin_dir, "initdb.exe")

  # Download binaries zip if needed
  if (!file.exists(zip_path)) {
    log_step("Downloading PostgreSQL binaries...")
    # Ensure base directory exists
    base_dir |>
      dir.create(recursive = TRUE, showWarnings = FALSE)
    # Prompt user to confirm download
    response <- readline(
      prompt = paste0(
        "The PostgreSQL binaries (~340MB) will be downloaded.\n",
        "Source: https://get.enterprisedb.com/postgresql/\n",
        "Is it okay to proceed? (yes/no): "
      )
    )
    if (response != "yes") {
      stop("Download cancelled.")
    }
    # Download zip
    utils::download.file(
      url = zip_src,
      destfile = zip_path,
      mode = "wb"
    )
  }

  # Unpack binaries if needed
  if (!file.exists(initdb)) {
    log_step("Unpacking PostgreSQL binaries...")
    utils::unzip(zip_path, exdir = base_dir)
  }

  # Initialize database
  if (!dir.exists(data_dir)) {
    log_step("Initializing PostgreSQL database...")
    # Ensure data directory exists
    data_dir |> dir.create(recursive = TRUE, showWarnings = FALSE)
    # Create temp. password file to pass to initdb
    pwfile <- tempfile("pgpw_")
    writeLines(password, pwfile)
    # Run initialize database command
    args <- c("-A", "md5", "--pwfile", pwfile) |>
      c("-U", user, "-D", data_dir)
    initdb |> system2(args = args, wait = TRUE)
    # remove temp. password file
    file_removed <- file.remove(pwfile)
  }
  invisible()
}

start_postgres_server <- function(
    db_name = "mydb",
    version = NULL,
    port = 5432,
    user = "postgres",
    password = "postgres") {
  if (is.null(version)) {
    version <- "17.0-1"
  }
  # Define persistent folders
  base_dir <- rappdirs::user_data_dir("postgres_handyr") |>
    file.path(version)
  data_dir <- file.path(base_dir, "data", db_name)
  bin_dir <- file.path(base_dir, "pgsql", "bin")
  pg_ctl <- file.path(bin_dir, "pg_ctl.exe")

  # Check if server is running
  args <- c("-D", data_dir, "status")
  status <- pg_ctl |>
    system2(args = args, stdout = TRUE, stderr = TRUE) |>
    suppressWarnings() # gives warning if server is not running

  # Start server if not running
  is_running <- "server is running" |>
    grepl(paste(status, collapse = "\n"))
  if (!is_running) {
    log_step("Starting PostgreSQL server...")
    args <- c("-D", data_dir, "-o", shQuote(paste0("-p ", port)), "start")
    pg_ctl |>
      system2(args = args, wait = TRUE)
  }
}

connect_to_postgres_db <- function(
    db_name = "mydb",
    version = NULL,
    port = 5432,
    user = "postgres",
    password = "postgres") {
  if (is.null(version)) {
    version <- "17.0-1"
  }
  # Start server if not running
  db_name |>
    start_postgres_server(
      version = version,
      port = port,
      user = user,
      password = password
    )
  # Wait for readiness
  log_step("Ensuring PostgreSQL can accept connections...")
  for (i in seq_len(15)) {
    ok <-
      {
        con <- .dbi_drivers$postgresql[[1]]() |>
          DBI::dbConnect(
            dbname = "postgres",
            host = "127.0.0.1",
            port = port,
            user = user,
            password = password
          )
        DBI::dbDisconnect(con)
        TRUE
      } |>
      on_error(.return = FALSE)

    if (ok) {
      break
    }
    if (i == 15) {
      stop("Timeout: PostgreSQL did not become ready.")
    }
    Sys.sleep(1)
  }

  .dbi_drivers$postgresql[[1]]() |>
    DBI::dbConnect(
      dbname = "postgres",
      host = "127.0.0.1",
      port = port,
      user = user,
      password = password
    )
}
