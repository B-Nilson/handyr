#' Apply a function over a rolling window
#'
#' `rolling` applies a funtion (`FUN`) over a moving window (see `.width` and `.direction`) for each element of `x` using [zoo::rollapply()].
#'
#' * Be sure to fill any gaps in `x` and ensure `x` is sorted, otherwise the window may be applied incorrectly.
#' * Values outside of the window are replaced with `.fill`.
#' * `.min_non_na` is passed to [do_if_enough()] to only apply `FUN` if enough values are present in the window.
#'
#' @param x A vector.
#' @param FUN A function to be applied to each window of `x`.
#'   Default is `mean`.
#' @param ... Additional arguments to be passed to `FUN`.
#' @param .width A numeric value indicating the width of the window.
#'   See [zoo::rollapply()] for more details.
#'   Default is `3`.
#' @param .direction A character string or numeric value indicating the direction of the window.
#'   Options are `"backward"` (1), `"forward"` (-1), or `"center"` (0).
#'   See the `align` argument in [zoo::rollapply()] for more details.
#'   Default is `"backward"`.
#' @param .fill (Optional) A single value to be used for filling in any values that are outside of the window.
#'   See [zoo::rollapply()] for more details.
#'   Default is `NA`.
#' @param .min_non_na (Optional) A single numeric value indicating the minimum number of observations required to
#'   compute a value.
#'   Default is `0`.
#'
#' @return A vector of the same length as `x` and type of the output of `FUN`.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' x |> rolling(mean, .width = 2)
#' x |> rolling(mean, .width = 2, .direction = "forward", .fill = -1)
#' @export
rolling <- function(x, FUN = mean, ..., .width = 3, .direction = "backward", .fill = NA, .min_non_na = 0) {
  rlang::check_installed("zoo")
  # Handle inputs
  stopifnot(is.function(FUN))
  stopifnot(is.numeric(.width), length(.width) == 1)
  stopifnot(is.numeric(.direction) | is.character(.direction), length(.direction) == 1)
  stopifnot(length(.fill) == 1)
  stopifnot(is.numeric(.min_non_na), length(.min_non_na) == 1)

  # translate .direction -> align for zoo
  align <- ifelse(
    .direction %in% c("backward", 1), "right",
    ifelse(.direction %in% c("forward", -1), "left",
      ifelse(.direction %in% c("center", 0), "center", NA)
    )
  )

  # Apply rolling function with specified window/fill
  # using `do_if_enough` to only apply function if enough values
  x |>
    zoo::rollapply(
      width = .width, align = align, fill = .fill,
      FUN = \(x) do_if_enough(x, FUN, .min_non_na = .min_non_na)
    )
}
