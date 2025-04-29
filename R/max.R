#' Wrapper for max to handle NAs cleanly
#'
#' @param ... numeric or character arguments (see [base::max]).
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @description
#'   `max` provides a wrapper for the built-in function [base::max]. This handles the edge case where
#'   all values provided to max are NA, which would otherwise throw an warning and return Inf.
#'   This is consitent with the behavior of [base::mean].
#'
#' @return The maximum value from the provided values, or NA if all values are NA.
#' @examples
#' data.frame(
#'   x = c(1, 2, 3, NA, NA, NA),
#'   y = c("a", "a", "a", "b", "b", "b")
#' ) |>
#'   dplyr::group_by(y) |>
#'   dplyr::summarise(max_x = max(x, na.rm = TRUE))
#'
#' @export
max <- function(..., na.rm = FALSE) {
  x <- unlist(list(...))
  # Handle inputs
  stopifnot(is.logical(na.rm), length(na.rm) == 1)

  # Handle NAs before max() to avoid warnings/Infinites
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Handle cases where max doesn't need to be called
  if (length(x) == 0 | all(is.na(x))) {
    return(ifelse(is.numeric(x), NA_real_, NA_character_))
  }

  # Find maximum
  base::max(x)
}
