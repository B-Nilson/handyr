#' Loop over a vector-like object and apply a function
#'
#' @param x Something iterable (a vector, list, etc).
#' @param FUN A function to be applied to each entry in `x`.
#' @param ... Additional arguments to be passed to `FUN` or to [future.apply::future_lapply()] if `.parallel = TRUE`.
#' @param .enumerate A logical value indicating if `i` should be passed to `FUN` alongside `x`.
#'   Default is `FALSE`.
#'   If `TRUE`, `FUN` is run as `FUN(x[[i]], i, ...)`, where `i` is the index of values in `x`.
#' @param .bind A logical value indicating whether to apply [dplyr::bind_rows()].
#'   Default is `FALSE`
#' @param .bind_id A single character string indicating the column name to use for the row index if `.bind = TRUE`.
#'   Default is `NULL` (don't add a row index column).
#' @param .name A logical value indicating if the output should be named after `x`. (i.e `names(out) <- x`)
#'   Default is `FALSE`.
#' @param .as_list A logical value indicating if the output should be a list (see [lapply()] / [future.apply::future_lapply()]) or a vector (see[sapply()] / [future.apply::future_sapply()]).
#'   Default is `NULL`, which is `FALSE` if `x` is a vector, `TRUE` otherwise.
#' @param .parallel A logical value indicating if the function should be run in parallel (see [future::multisession()]).
#'   Default is `FALSE`.
#' @param .workers A single numeric value indicating the number of workers to run in parallel if `.parallel = TRUE`.
#'   Default is `NULL` which uses all available cores (see [parallel::detectCores()]).
#' @param .plan A string indicating the strategy to use if `.parallel = TRUE`.
#'   Default is `"multisession"` (see [future::plan()]).
#' @param .parallel_cleanup A logical value indicating if the parallel plan should be reset to sequential using `future::plan("sequential")` if `.parallel = TRUE`.
#'   Default is `TRUE`.
#' @param .quiet A logical value indicating if the output should be invisible (no messages/warnings).
#'   Default is `FALSE`.
#'
#' @description
#' `for_each` provides a simple way to loop over a vector and apply a function with useful postprocessing.
#'
#' @return a list of the output of `FUN` iterated over `x` which:
#'   if `.bind = TRUE`: is bound rowwise into a data frame using [dplyr::bind_rows()]
#'   if `.name = TRUE`: has names set to `x` using `names(out) <- x`
#'   if `.quiet = TRUE`: is invisible
#' @export
#'
#' @examples
#' 1:3 |> for_each(\(value) value + 1)
#' c(7, 7, 7) |> for_each(\(value, i) value + i, .enumerate = TRUE)
#'
#' list(
#'   data.frame(x = 1:3),
#'   data.frame(x = 4:6)
#' ) |>
#'   for_each(
#'     \(dat) dat |> dplyr::mutate(y = x + 1),
#'     .bind = TRUE
#'   )
#'
#' c("bread", "jam") |>
#'   for_each(
#'     \(value) paste("eat", value),
#'     .name = TRUE
#'   )
#'
#' values <- 1:3 |>
#'   for_each(\(value) value + 1, .parallel = TRUE, .workers = 2, .as_list = TRUE)
#'
#' values <- 1:3 |>
#'   for_each(\(value) message(value + 1), .quiet = TRUE)
for_each <- function(x, FUN, ..., .enumerate = FALSE, .bind = FALSE, .bind_id = NULL, .name = FALSE, .as_list = NULL, .parallel = FALSE, .workers = NULL, .plan = "multisession", .parallel_cleanup = TRUE, .quiet = FALSE) {
  # Handle inputs
  stopifnot(is.function(FUN))
  stopifnot(is.logical(.enumerate), length(.enumerate) == 1)
  stopifnot(is.logical(.bind), length(.bind) == 1)
  stopifnot(is.character(.bind_id) | is.null(.bind_id))
  stopifnot(length(.bind_id) == 1 | is.null(.bind_id))
  stopifnot(is.logical(.name), length(.name) == 1)
  stopifnot(is.logical(.as_list) | is.null(.as_list))
  stopifnot(length(.as_list) == 1 | is.null(.as_list))
  stopifnot(is.logical(.parallel), length(.parallel) == 1)
  stopifnot(is.numeric(.workers) | is.null(.workers))
  stopifnot(length(.workers) == 1 | is.null(.workers))
  stopifnot(is.character(.plan) | is.function(.plan) | is.null(.plan)) # TODO: restrict inputs more?
  stopifnot(length(.plan) == 1 | is.null(.plan))
  stopifnot(is.logical(.parallel_cleanup), length(.parallel_cleanup) == 1)
  stopifnot(is.logical(.quiet), length(.quiet) == 1)

  # Handle .as_list being NULL
  if (is.null(.as_list)) .as_list <- !(is.vector(x) & !is.list(x)) # if x is vector, return vector, otherwise return list

  # Setup parallel if desired
  # TODO: handle potential side effect here if user already had a plan() going
  if (.parallel & length(x) > 1) {
    rlang::check_installed("future.apply", reason = "`.parallel` set to `TRUE`")
    if (is.null(.workers)) .workers <- parallel::detectCores()
    if (!is.null(.plan)) future::plan(.plan, workers = .workers)
    apply_fun <- ifelse(.as_list, future.apply::future_lapply, future.apply::future_sapply)
  } else {
    apply_fun <- ifelse(.as_list, lapply, sapply)
  }

  # Run x (and i if .enumerate) through function
  if (.quiet) {
    if (.enumerate) {
      out <- suppressWarnings(suppressMessages(invisible(
        seq_along(x) |> apply_fun(\(i) FUN(x[[i]], i, ...))
      )))
    } else {
      out <- suppressWarnings(suppressMessages(invisible(
        x |> apply_fun(FUN, ...)
      )))
    }
  } else {
    if (.enumerate) {
      out <- seq_along(x) |> apply_fun(\(i) FUN(x[[i]], i, ...))
    } else {
      out <- x |> apply_fun(FUN, ...)
    }
  }

  # Stop running in parallel
  if (.parallel & .parallel_cleanup) future::plan(future::sequential)
  # Use x as names if desired
  if (.name) names(out) <- x
  # Bind rowwise (and potentially add list index column) if desired
  if (.bind) out <- out |> dplyr::bind_rows(.id = .bind_id)

  # Return if not quiet, otherwise return invisibly
  if (!.quiet) {
    return(out)
  }
  invisible(out)
}
