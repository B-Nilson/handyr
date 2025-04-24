#' Clamp a value to a certain range
#'
#' @param x A numeric vector of value(s)
#' @param range A numeric vector of length two to clamp `x` to
#' @return A numeric vector of `x` with values less than `min(range)` set to `min(range)` and values greater than `max(range)` set to `max(range)`
#' @export
clamp = function(x, range) {
  pmin(pmax(x, min(range)), max(range))
}
