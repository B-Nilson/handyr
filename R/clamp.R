#' Clamp a value to a certain range
#'
#' @param x A numeric vector of value(s)
#' @param range A numeric vector of length two to clamp `x` to. If `range[1]` is `NA`, then `min(x)` will be used. If `range[2]` is `NA`, then `max(x)` will be used
#' @return A numeric vector of `x` with values less than `min(range)` set to `min(range)` and values greater than `max(range)` set to `max(range)`
#'
#' @examples
#' clamp(1:5, range = c(2, 4))
#' clamp(1:5, range = c(-1, 8))
#' @export
clamp <- function(x, range = c(NA, NA)) {
  # input checks
  stopifnot(length(range) == 2)
  stopifnot(is.numeric(x), is.numeric(range))
  # set NA range values to min/max
  if (is.na(range[1])) range[1] <- min(x, na.rm = TRUE)
  if (is.na(range[2])) range[2] <- max(x, na.rm = TRUE)
  # ensure range is sorted
  range <- sort(range)

  x |>
    pmax(range[1]) |>
    pmin(range[2])
}
