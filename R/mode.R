#' Calculate the stitistical mode (most common value)
#'
#' @param ... numeric or character arguments (see [base::max]).
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @description
#'   `mode` determines the most common value in a vector while cleanly handling NAs
#'
#' @return The most common value from the provided values, or NA if all values are NA.
#' @examples
#' mode(c(1, 1, 1:3))
#'
#' @export
mode <- function(..., na.rm = FALSE) {
  x <- unlist(list(...))
  # Handle NAs before calculating
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Handle cases where mode doesn't need to be called
  if (length(x) == 0 | all(is.na(x))) {
    return(ifelse(is.numeric(x), NA_real_, NA_character_))
  }
  # Get most common value
  ux <- unique(x)
  max_idx <- match(x, ux) |>
    tabulate() |>
    which.max()
  ux[max_idx]
}
