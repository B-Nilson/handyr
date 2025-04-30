#' Apply a function if enough values are non-NA
#'
#' @description
#'   `do_if_enough`:
#'    * checks if enough non-NA values are in `x`. (see `.min_non_na`)
#'    * if so, applies a function (`FUN`) to `x`.
#'    * if not, returns `.return`.
#' 
#' @param x A vector (numeric, character, etc).
#' @param FUN A function to be applied to `x`.
#' @param ... Additional arguments to be passed to `FUN`.
#' @param .min_non_na The minimum length of `x` (ignoring NAs) to be considered enough.
#' @param .return What to return instead of the output of `FUN` if not enough non-NA values.
#'
#' @return The output of FUN applied to x if enough values are provided, NA otherwise
#' 
#' @export
#'
#' @examples
#' do_if_enough(c(1, 2, 3), mean, .min_non_na = 2)
#' do_if_enough(c(1, 2, NA), mean, .min_non_na = 2)
#' do_if_enough(c(1, NA, NA), mean, .min_non_na = 2)
#' do_if_enough(c(1, NA, NA), mean, .min_non_na = 2, .return = -1)
do_if_enough <- function(x, FUN, ..., .min_non_na = 1, .return = NA) {
  # Handle inputs
  stopifnot(is.vector(x), !is.list(x))
  stopifnot(is.function(FUN))
  stopifnot(
    is.numeric(.min_non_na), length(.min_non_na) == 1, .min_non_na >= 0,
    !is.na(.min_non_na), is.finite(.min_non_na)
  )

  # Apply function if min_length low enough to not matter
  if (.min_non_na <= 1) {
    return(FUN(x, na.rm = TRUE, ...))
  }
  # Return .return if too few values
  is_enough <- sum(!is.na(x)) >= .min_non_na
  if (!is_enough) {
    return(.return)
  }
  
  # Apply function ignoring NAs
  FUN(x, na.rm = TRUE, ...)
}
