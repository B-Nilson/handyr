#' Get the most common value
#'
#' `mode` determines the statistical mode (the most common value) of one or more vectors
#' 
#' @inheritParams max
#' 
#' @return A single value with the same type as `...`
#' 
#' @examples
#' mode(c("A", "B", "A")) # Returns "A"
#' mode(1:3, 3:6, 2:4, NA) # Returns 3
#' mode(1, 1:3, c(NA, NA, NA)) # Returns NA
#' mode(1, 1:3, c(NA, NA, NA), na.rm = TRUE) # Returns 1
#' 
#' @export
mode <- function(..., na.rm = FALSE) {
  x <- unlist(list(...))
  # Handle inputs
  stopifnot(is.logical(na.rm), length(na.rm) == 1)

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
