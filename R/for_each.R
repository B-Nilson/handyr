#' Loop over a vector and apply a function with useful postprocessing
#'
#' @param input A vector of values to be iterated over.
#' @param FUN A function to be applied to each value in `input`.
#' @param ... Any other arguments to be passed to `FUN` or to [future.apply::future_lapply()] if `.parallel = TRUE`.
#' @param .bind (Optional) A single logical (TRUE/FALSE) value indicating if the output should be bound rowwise.
#'   Default is `FALSE`
#' @param .name (Optional) A single logical (TRUE/FALSE) value indicating if the output should be named after the input.
#'   Default is `FALSE`
#' @param .parallel (Optional) A single logical (TRUE/FALSE) value indicating if the function should be run in parallel (see [future::multisession()]).
#'   Default is `FALSE`
#' @param .workers (Optional) A single numeric value indicating the number of workers to run in parallel.
#'   Default is all available cores (see [parallel::detectCores()])
#' @param .invisible (Optional) A single logical (TRUE/FALSE) value indicating if the output should be invisible (no messages/warnings).
#'
#' @description
#' `for_each` provides a simple way to loop over a vector and apply a function with useful postprocessing.
#'
#' @family Utilities
#'
#' @return the output of `FUN` iterated over `input`, optionally bound rowwise, named after `input`, or invisible.
#' @export
#'
#' @examples
#' values <- 1:3
#' values |> for_each(\(value) value + 1)
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
#'   for_each(\(value) message(value + 1), .invisible = TRUE)
for_each <- function(input, FUN, ..., .bind = FALSE, .name = FALSE, .parallel = FALSE, .workers = parallel::detectCores(), .invisible = FALSE) {
  if (.parallel) {
    rlang::check_installed("future.apply", reason = "`.parallel` set to `TRUE`")
    future::plan(future::multisession, workers = .workers) # Run in parallel if desired
    lapply_fun <- future.apply::future_lapply
  } else {
    lapply_fun <- lapply
  }

  # Run input through function
  if (.invisible) {
    out <- suppressWarnings(suppressMessages(invisible(input |>
      lapply_fun(FUN, ...))))
  } else {
    out <- input |> lapply_fun(FUN, ...)
  }

  if (.parallel) future::plan(future::sequential) # Stop running in parallel

  if (.name) names(out) <- input # Use input as names if desired

  if (.bind) out <- out |> dplyr::bind_rows() # Bind rowise if desired

  if (.invisible) {
    invisible(out)
  } else {
    return(out)
  }
}
