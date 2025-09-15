#' Read data from a database
#'
#' `read_from_database` takes a database connection and table name, applies a dplyr-based query, and returns either a lazy-table or a data frame of the query results.
#'
#' @param db A database connection or the path to a database file (if sqlite or duckdb file extension).
#' @param table_name A character string specifying the table to read from.
#' @param query_fun A function taking a single argument (the data.frame-like object to be queried) and returning a data frame.
#'   Default is the identity function, which simply returns the input data frame.
#'   Most `dplyr` functions will be implemented here (see [dbplyr] for details).
#'   (e.g. `query_fun = \(df) df |> dplyr::select(column1, column2)`)
#'   Anything else beyond variable names needs to be prefaced with `!!` (e.g. `... |>  dplyr::filter(month |> dplyr::between(!!select_months[1], !!select_months[2]))`).
#' @param collect A logical value indicating whether to use [dplyr::collect()] to fetch the data from the database.
#'   Default is `TRUE`.
#'
#' @return A data frame containing the data from the specified table in the database.
#' @export
read_from_database <- function(
  db,
  table_name,
  query_fun = \(df) df,
  collect = TRUE
) {
  stopifnot((is.character(db) & length(db) == 1) | is_db_connection(db))
  stopifnot(is.character(table_name), length(table_name) == 1)
  stopifnot(is.function(query_fun))
  stopifnot(is.logical(collect), length(collect) == 1)
  rlang::check_installed("DBI")
  rlang::check_installed("dbplyr")

  # Handle db path instead of connection
  if (is.character(db)) {
    db <- db_conn_from_path(db)
  }

  # Connect to table and build query
  query <- db |>
    dplyr::tbl(table_name) |>
    query_fun()

  # Either collect results or return lazy table of query
  if (collect) {
    output <- dplyr::collect(query)
  } else {
    output <- query
  }
  return(output)
}
