#' Apply a function if enough values are provided
#'
#' @param x A vector of values
#' @param FUN A function to be applied to x
#' @param ... Additional arguments to be passed to FUN
#' @param .min_non_na The minimum length of `x` (ignoring NAs) to be considered enough
#'
#' @description
#'   `do_if_enough` provides a simple way to apply a function if enough values are provided.
#' @return The output of FUN applied to x if enough values are provided, NA otherwise
#' @export
#'
#' @examples
#' do_if_enough(c(1, 2, 3), mean, .min_non_na = 2)
#' do_if_enough(c(1, 2, NA), mean, .min_non_na = 2)
#' do_if_enough(c(1, NA, NA), mean, .min_non_na = 2)
do_if_enough <- function(x, FUN, ..., .min_non_na = 1) {
  # Handle inputs
  stopifnot(is.function(FUN))
  stopifnot(
    is.numeric(.min_non_na), length(.min_non_na) == 1, .min_non_na >= 0,
    !is.na(.min_non_na), is.finite(.min_non_na)
  )

  # Apply function if min_length low enough to not matter
  if (.min_non_na <= 1) {
    return(FUN(x, na.rm = TRUE, ...))
  }
  # Return NA if too few values
  is_enough <- sum(!is.na(x)) >= .min_non_na
  if (!is_enough) {
    return(NA)
  }
  
  # Apply function ignoring NAs
  FUN(x, na.rm = TRUE, ...)
}
