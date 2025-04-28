#' Wrapper for min to handle NAs cleanly
#'
#' @param ... numeric or character arguments (see [base::min]).
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @description
#'   `min` provides a wrapper for the built-in function [base::min]. This handles the edge case where
#'   all values provided to min are NA, which would otherwise throw a warning and return Inf.
#'   This is consistent with the behavior of [base::mean].
#'
#' @return The minimum value from the provided values, or NA if all values are NA.
#' @examples
#' data.frame(
#'   x = c(1, 2, 3, NA, NA, NA),
#'   y = c("a", "a", "a", "b", "b", "b")
#' ) |>
#'   dplyr::group_by(y) |>
#'   dplyr::summarise(min_x = min(x, na.rm = TRUE))
#'
#' @export
min <- function(..., na.rm = FALSE) {
  x <- unlist(list(...))
  # Handle NAs before min() to avoid warnings/Infinites
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Handle cases where min doesn't need to be called
  if (length(x) == 0 | all(is.na(x))) {
    ifelse(is.numeric(x), return(NA_real_), return(NA_character_))
  }
  base::min(x)
}

