#' Get the maximum/minimum value while handling NAs cleanly
#'
#' `max`/`min` provides a wrapper for the built-in functions [base::max]/[base::min] that handles NAs the same as [base::mean].
#'
#' This handles the case where all values provided to max/min are NA, which normally returns Inf/-Inf and throws a warning (see examples).
#'
#' @param ... numeric, character, Date, or POSIXct vectors (see [base::max]).
#' @param na.rm A logical value indicating whether NA values should be removed.
#'
#' @return A single value with the same type as `...`
#'
#' @examples
#' # Edge case where all values are NA
#' base::max(c(NA, NA, NA)) # Returns NA
#' base::max(c(NA, NA, NA), na.rm = TRUE) # Returns -Inf and throws warning
#' base::min(c(NA, NA, NA), na.rm = TRUE) # Returns Inf and throws warning
#' max(c(NA, NA, NA)) # Returns NA
#' max(c(NA, NA, NA), na.rm = TRUE) # Returns NA
#' min(c(NA, NA, NA), na.rm = TRUE) # Returns NA
#'
#' # Example usage with typical dplyr pipeline
#' data.frame(
#'   x = c(1, 2, 3, NA, NA, NA),
#'   y = c("a", "a", "a", "b", "b", "b")
#' ) |>
#'   dplyr::group_by(y) |>
#'   dplyr::summarise(
#'     base_max_x = base::max(x, na.rm = TRUE), # for comparison
#'     base_min_x = base::min(x, na.rm = TRUE), # for comparison
#'     max_x = max(x, na.rm = TRUE),
#'     min_x = min(x, na.rm = TRUE)
#'   )
#'
#' # Dates are supported too
#' max(as.Date(c("2022-01-01", "2022-01-03")), na.rm = TRUE) # Returns a Date
#'
#' @export
max <- function(..., na.rm = FALSE) {
  x <- c(...)
  if (is.null(x)) {
    x <- logical(0) # no/`NULL` inputs should still return a single NA
  }
  # Handle inputs
  stopifnot(is.logical(na.rm), length(na.rm) == 1)

  # Handle NAs before max() to avoid warnings/Infinites
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Handle cases where max doesn't need to be called
  not_needed <- length(x) == 0 | all(is.na(x))
  if (not_needed) {
    # Return an NA of the same type/class as `x`
    return(x[NA_integer_])
  }

  # Find maximum
  base::max(x)
}

#' @rdname max
#' @export
min <- function(..., na.rm = FALSE) {
  x <- c(...)
  if (is.null(x)) {
    x <- logical(0) # no/`NULL` inputs should still return a single NA
  }
  # Handle inputs
  stopifnot(is.logical(na.rm), length(na.rm) == 1)

  # Handle NAs before min() to avoid warnings/Infinites
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Handle cases where min doesn't need to be called
  not_needed <- length(x) == 0 | all(is.na(x))
  if (not_needed) {
    # Return an NA of the same type/class as `x`
    return(x[NA_integer_])
  }

  # Find minimum
  base::min(x)
}
