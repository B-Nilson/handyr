# TODO: add tests
#' Swap out values in a vector
#'
#' @param x Vector of values to be have certain values swapped out.
#' @param what One or more values to be replaced with `with` throughout `x`.
#' @param with A single value to replace `what` for throughout `x`.
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
swap <- function(x, what, with) {
  if (any(is.na(what))) {
    x[is.na(x)] <- with
  }
  if (any(is.infinite(what))) {
    x[is.infinite(x)] <- with
  }
  x[x %in% what] <- with
  return(x)
}
# Wrappers for swap()
swap_na <- function(x, with = -99) swap(x, NA, with)
swap_inf <- function(x, with = NA) swap(x, Inf, with)