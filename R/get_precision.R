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
#' get_precision(c(1e-7, 1e16)) # tiny/huge numbers are handled too
get_precision <- function(x) {
  stopifnot(is.numeric(x))
  # Convert to a full-precision string (15 significant digits, e.g. "1e-07")
  # (`format(x, scientific = FALSE)` cannot be used here as it emits
  # floating-point noise and still uses scientific notation for extremes)
  s <- sprintf("%.15g", x)

  # Split off the exponent so scientific notation is handled
  # (e.g. "1e-07" -> exponent -7 -> 7 decimal places, "1e+16" -> 0)
  has_exp <- grepl("e", s)
  exponent <- rep(0L, length(s))
  exponent[has_exp] <- sub(".*e([+-]?[0-9]+)$", "\\1", s[has_exp]) |>
    as.integer()
  mantissa <- sub("e[+-]?[0-9]+$", "", s)

  # Append a decimal if none present
  has_decimal <- grepl(pattern = "\\.", x = mantissa)
  mantissa[!has_decimal] <- paste0(mantissa[!has_decimal], ".")

  # Get number of characters after decimal, adjusted for the exponent
  post_decimal <- sub(pattern = ".*\\.", replacement = "", x = mantissa)
  post_decimal <- ifelse(is.na(x), NA, post_decimal)
  pmax(nchar(post_decimal) - exponent, 0L)
}
