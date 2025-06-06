#' Apply a function if a condition is met
#'
#' @param x Something that `.do` can be applied to.
#' @param .do A function to be applied to `x`.
#' @param .if A logical value indicating if `.do` should be applied or not.
#' @param ... Additional arguments to be passed to `.do`.
#' @param .return What to return instead of the output of `.do` if `.if == FALSE`.
#'
#' @return The output of `.do` applied to `x` if `.if == TRUE`, `.return` otherwise
#'
#' @export
#'
#' @examples
#' c(1, 2, 3) |> do_if(.do = mean, .if = TRUE)
#' c(1, 2, 3) |> do_if(.do = mean, .if = FALSE)
#' c(1, 2, 3) |> do_if(.do = mean, .if = FALSE, .return = -1)
do_if <- function(x, .do, .if = TRUE, ..., .return = NA) {
  # Handle inputs
  stopifnot(is.function(.do))
  stopifnot(is.logical(.if), length(.if) == 1, !is.na(.if))

  # Return .return if not is_true, otherwise apply function
  if (!.if) {
    return(.return)
  } else {
    .do(x, ...)
  }
}

#' Apply a function if enough values are non-NA
#'
#' @description
#'   `do_if_enough`:
#'    * checks if enough non-NA values are in `x`. (see `.min_non_na`)
#'    * if so, applies a function (`FUN`) to `x`.
#'    * if not, returns `.return`. (see [do_if()] for more details)
#'
#' @param x A vector (numeric, character, etc).
#' @param FUN A function to be applied to `x`.
#' @param ... Additional arguments to be passed to `FUN`.
#' @param .min_non_na The minimum length of `x` (ignoring NAs) to be considered enough.
#' @param .return What to return instead of the output of `FUN` if not enough non-NA values.
#'
#' @return The output of FUN applied to x if enough values are provided, `.return` otherwise
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

  # Remove NAs
  x <- x[!is.na(x)]

  # Apply function if min_length low enough to not matter
  if (.min_non_na <= 1 & length(x) >= .min_non_na) {
    return(FUN(x, ...))
  }

  # Return .return if too few values, apply function otherwise
  x |> do_if(
    .do = FUN, ...,
    .if = length(x) >= .min_non_na,
    .return = .return
  )
}
