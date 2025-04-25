#' Truncate a numeric vector to a certain number of decimal places
#'
#' @param x A numeric vector to be truncated
#' @param digits A single numeric value indicating the number of decimal places to round to
#'   Default is `0`
#'
#' @return A numeric vector of `x` truncated to `digits` decimal places
#' @export
truncate <- function(x, digits = 0) {
  trunc(x * 10^digits) / 10^digits
}