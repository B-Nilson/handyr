#' Replace out-of-range values with the nearest in-range value
#'
#' `clamp` sets values less than `min(range)` to `min(range)` and values greater than `max(range)` to `max(range)`
#'
#' @param x A numeric vector.
#' @param range A numeric vector of length two to clamp `x` to.
#'   * If `range[1]` is `NA` (default), then `min(x)` will be used.
#'   * If `range[2]` is `NA` (default), then `max(x)` will be used.
#'
#' @return
#' A numeric vector equal to `x` with:
#'  * values less than `min(range)` set to `min(range)`
#'  * values greater than `max(range)` set to `max(range)`
#'
#' @export
#'
#' @examples
#' clamp(1:5, range = c(2, NA)) # 2, 2, 3, 4, 5
#' clamp(1:5, range = c(2, 4)) # 2, 2, 3, 4, 4
#' clamp(1:5, range = c(-1, 1)) # 1, 1, 1, 1, 1
clamp <- function(x, range = c(NA, NA)) {
  # Handle inputs
  stopifnot(is.numeric(x), is.vector(x), !is.list(x))
  stopifnot(length(range) == 2, is.numeric(range) | all(is.na(range)))

  # set NA range values to min/max
  if (is.na(range[1])) range[1] <- min(x, na.rm = TRUE)
  if (is.na(range[2])) range[2] <- max(x, na.rm = TRUE)
  # ensure range is sorted
  range <- sort(range)

  # clamp values
  x |>
    pmax(range[1]) |>
    pmin(range[2])
}
