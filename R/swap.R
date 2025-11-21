#' Swap out values in a vector
#'
#' @param x Vector of values to be have certain values swapped out.
#' @param what One or more values to be replaced with `with` throughout `x`, 
#'   or a logical vector with the same length as `x` indicating which values to replace.
#' @param with A single value to replace `what` for throughout `x`, or a vector with the same length as `x` indicating which values to insert for each value in `x`.
#'
#' @description
#' `swap` provides a simple way to switch out certain values in a vector. It is useful for replacing NA's, Infinites, and erroneous values.
#'
#' @family Utilities
#'
#' @return a vector of `x` where all instances of `what` are replaced with `with`
#' @export
#'
#' @examples
#' swap(c(-20:20, NA), what = NA, with = -1)
#' swap(c(-20:20, Inf), what = Inf, with = NA)
#' swap(c(-20:20), what = Inf, with = NA)
#' x <- 1:10
#' swap(x, what = x < 5, with = -1)
#' y <- 10:1
#' swap(x, what = x < 5, with = y)
swap <- function(x, what, with) {
  # Handle inputs
  if (length(x) == 0) {
    return(x)
  }
  if (all(is.na(what))) {
    what <- NA_character_ # in case `NA` (NA_logical_) is passed
  }
  stopifnot(length(what) >= 1)
  stopifnot("`what` must have the same length as `x` if `what` is logical." = !is.logical(what) | (is.logical(what) & length(what) == length(x)))
  stopifnot(length(with) == 1 | length(with) == length(x))

  # Swap values
  if (is.logical(what)) {
    if (length(with) == length(x)) {
      with <- with[what]
    }
    x[what] <- with
  }else {
    if (length(with) == length(x)) {
      with <- with[x %in% what]
    }
    x[x %in% what] <- with
  }
  return(x)
}
