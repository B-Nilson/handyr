#' Apply a function if enough values are provided
#'
#' @param x A vector of values
#' @param FUN A function to be applied to x
#' @param ... Additional arguments to be passed to FUN
#' @param .min_length The minimum length of `x` to be considered enough
#'
#' @description
#'   `do_if_enough` provides a simple way to apply a function if enough values are provided.
#' @return The output of FUN applied to x if enough values are provided, NA otherwise
#' @export
#'
#' @examples
#' do_if_enough(c(1, 2, 3), mean, min_length = 2)
#' do_if_enough(c(1, 2, NA), mean, min_length = 2)
#' do_if_enough(c(1, NA, NA), mean, min_length = 2)
do_if_enough <- function(x, FUN, ..., .min_length = 0) {
  # Handle inputs
  stopifnot(is.function(FUN))
  stopifnot(
    is.numeric(.min_length), length(.min_length) == 1, .min_length >= 0,
    !is.na(.min_length), is.finite(.min_length)
  )
  if(.min_length == 0) {
    return(FUN(x, na.rm = TRUE, ...))
  }
  is_enough <- sum(!is.na(x)) >= .min_length
  if (is_enough) {
    FUN(x, na.rm = TRUE, ...)
  } else {
    return(NA)
  }
}
