#' Get the number of decimal places in a numeric vector
#'
#' @param x A numeric vector.
#' @return
#' A numeric vector with the same length as `x`, giving the number of decimal places for each element in `x`.
#'
#' @export
#' @examples
#' get_precision(1:5)
#' get_precision(c(1.2, 1.23, 1.234))
get_precision <- function(x) {
  stopifnot(is.numeric(x))
  # Convert to character
  s <- as.character(x, scientific = FALSE)

  # Append a decimal if none present
  has_decimal <- grepl(pattern = "\\.", x = s)
  s[!has_decimal] <- paste0(s[!has_decimal], ".")

  # Get number of characters after decimal
  post_decimal <- sub(pattern = ".*\\.", replacement = "", x = s)
  post_decimal <- ifelse(is.na(x), NA, post_decimal)
  nchar(post_decimal)
}