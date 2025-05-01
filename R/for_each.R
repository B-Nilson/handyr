#' Loop over a vector and apply a function with useful postprocessing
#'
#' @param x Something iterable (a vector, list, etc).
#' @param FUN A function to be applied to each entry in `input`.
#' @param ... Additional arguments to be passed to `FUN` or to [future.apply::future_lapply()] if `.parallel = TRUE`.
#' @param .bind A logical value indicating whether to apply [dplyr::bind_rows()].
#'   Default is `FALSE`
#' @param .name A logical value indicating if the output should be named after `x`. (i.e `names(out) <- x`)
#'   Default is `FALSE`.
#' @param .parallel A logical value indicating if the function should be run in parallel (see [future::multisession()]).
#'   Default is `FALSE`.
#' @param .workers A single numeric value indicating the number of workers to run in parallel if `.parallel = TRUE`.
#'   Default is `NULL` which uses all available cores (see [parallel::detectCores()]).
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
#'   for_each(\(value) value + 1, .parallel = TRUE, .workers = 2)
#'
#' values <- 1:3 |>
#'   for_each(\(value) message(value + 1), .quiet = TRUE)
for_each <- function(x, FUN, ..., .bind = FALSE, .name = FALSE, .parallel = FALSE, .workers = NULL, .quiet = FALSE) {
  # Handle inputs
  stopifnot(is.function(FUN))
  stopifnot(is.logical(.bind), length(.bind) == 1)
  stopifnot(is.logical(.name), length(.name) == 1)
  stopifnot(is.logical(.parallel), length(.parallel) == 1)
  stopifnot(is.numeric(.workers), length(.workers) == 1)
  stopifnot(is.logical(.quiet), length(.quiet) == 1)

  # Setup parallel if desired
  # TODO: handle potential side effect here if user already had a plan() going
  if (.parallel & length(x) > 1) {
    rlang::check_installed("future.apply", reason = "`.parallel` set to `TRUE`")
    if (is.null(.workers)) .workers <- parallel::detectCores()
    future::plan(future::multisession, workers = .workers)
    lapply_fun <- future.apply::future_lapply
  } else {
    lapply_fun <- lapply
  }

  # Run x through function
  if (.quiet) {
    out <- suppressWarnings(suppressMessages(invisible(
      x |> lapply_fun(FUN, ...)
    )))
  } else {
    out <- x |> lapply_fun(FUN, ...)
  }

  # Stop running in parallel
  # TODO: handle potential side effect here if user already had a plan() going
  if (.parallel) future::plan(future::sequential)
  # Use x as names if desired
  if (.name) names(out) <- x
  # Bind rowwise if desired
  if (.bind) out <- out |> dplyr::bind_rows()

  # Return if not quiet, otherwise return invisibly
  if (!.quiet) {
    return(out)
  }
  invisible(out)
}
