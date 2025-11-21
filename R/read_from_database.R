#' Read data from a database
#'
#' `read_from_database` takes a database connection and table name, applies a dplyr-based query, and returns either a lazy-table or a data frame of the query results.
#'
#' @param db A database connection or the path to a database file (if sqlite or duckdb file extension).
#' @param table_name A character string specifying the table to read from.
#' @param query_fun A function taking a single argument (the data.frame-like object to be queried) and returning a data frame.
#'   Default is `NULL`, which does not apply a query fun.
#'   Most `dplyr` functions will be implemented here (see [dbplyr] for details).
#'   (e.g. `query_fun = \(df) df |> dplyr::select(column1, column2)`)
#'   Anything else beyond variable names needs to be prefaced with `!!` (e.g. `... |>  dplyr::filter(month |> dplyr::between(!!select_months[1], !!select_months[2]))`).
#' @param collect A logical value indicating whether to use [dplyr::collect()] to fetch the data from the database.
#'   Will be overidden if `pull` is not `NULL`.
#'   Default is `TRUE`.
#' @param pull A single character string to use with [dplyr::pull()] to pull a column after applying `query_fun`.
#'   Default is `NULL`, which does not pull any columns.
#'
#' @return
#' `query_fun` applied to the table `table_name` in `db` where if:
#'  * `collect` is `TRUE` (the default), returns a tibble (data.frame)
#'  * `collect` is `FALSE`, returns a lazy table of the query
#'  * **Unless** `pull` is a column name, returns a vector of the specified column
#' @export
read_from_database <- function(
  db,
  table_name,
  query_fun = NULL,
  collect = TRUE,
  pull = NULL
) {
  stopifnot((is.character(db) & length(db) == 1) | is_db_connection(db))
  stopifnot(is.character(table_name), length(table_name) == 1)
  stopifnot(is.function(query_fun) | is.null(query_fun))
  stopifnot(is.logical(collect), length(collect) == 1)
  stopifnot((is.character(pull) & length(pull) == 1) | is.null(pull))
  rlang::check_installed("DBI")
  rlang::check_installed("dbplyr")

  # Handle db path instead of connection
  if (is.character(db)) {
    db <- connect_to_database(
      name = basename(db),
      path = dirname(db)
    )
  }

  # Connect to table and build query
  query <- db |>
    dplyr::tbl(table_name)
  if (!is.null(query_fun)) {
    query <- query_fun(query)
  }

  # Either pull the desired column, collect results or return lazy table of query
  if (!is.null(pull)) {
    output <- query |> dplyr::pull(pull)
  } else if (collect) {
    output <- dplyr::collect(query)
  } else {
    output <- query
  }
  return(output)
}
