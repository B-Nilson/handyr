#' Shorten a number to a string with a unit prefix (e.g. "1K", "10M", "100B")
#'
#' Provided numbers will be divided by their highest met threshold and rounded to the specified number of decimal places.
#' These values will then be converted to characters and appended with the appropriate threshold symbol.
#'
#' The reverse can be performed by setting `reverse = TRUE` and providing a character vector to `x` (i.e. "1K" -> 1000).
#'
#' @param x A vector of numeric values to shorten, or character values to lengthen (if `reverse = TRUE`).
#' @param decimals The number of decimal places to round `x` to if `reverse = FALSE`. Default is `2`.
#' @param reverse A logical indicating if the thresholds should be used in reverse order.
#' @param thresholds A named list of thresholds.
#'   The names of the list are used as the unit prefix.
#'   The values of the list must be powers of 10.
#'   Default is `list("K" = 1e3, "M" = 1e6, "B" = 1e9, "T" = 1e12)`.
#' @return A shortened string representation of `x` if `reverse = FALSE`,
#'  or a numeric vector of lengthened `x` values if `reverse = TRUE`.
#' @export
#' @examples
#' x <- c(
#'    1,
#'    100,
#'    1000,
#'    10000,
#'    100000,
#'    1000000,
#'    10000000,
#'    100000000,
#'    1000000000,
#'    10000000000
#'  )
#' shorten_number(x)
#'
#' x <- c("1", "100", "1K", "10K", "100K", "1M", "10M", "100M", "1B", "10B")
#' shorten_number(x, reverse = TRUE)
shorten_number <- function(
  x,
  decimals = 2,
  reverse = FALSE,
  thresholds = list("K" = 1e3, "M" = 1e6, "B" = 1e9, "T" = 1e12)
) {
  stopifnot(is.numeric(x) | is.character(x), length(x) > 0)
  stopifnot(
    is.numeric(decimals),
    length(decimals) == 1
  )
  stopifnot(is.logical(reverse), length(reverse) == 1)
  stopifnot(
    "all thresholds must be powers of 10" = all(unlist(thresholds) %% 10 == 0)
  )
  stopifnot(
    "thresholds must be named with symbols to replace them with" = length(names(
      thresholds
    )) >
      0
  )

  # Ensure thresholds are in ascending order
  thresholds <- thresholds[order(unlist(thresholds))]

  # For each symbol, replace all values greater than or equal to the threshold with the symbol
  # (or the reverse if reverse = TRUE)
  output <- if (reverse) as.numeric(x) else as.character(x)
  for (symbol in names(thresholds)) {
    threshold <- thresholds[[symbol]]
    if (!reverse) {
      has_symbol <- x >= threshold
      output[has_symbol] <-
        (x[has_symbol] / threshold) |>
        round(digits = decimals) |>
        paste0(symbol)
    } else {
      has_symbol <- x |> grep(pattern = symbol)
      output[has_symbol] <- x[has_symbol] |>
        gsub(
          pattern = symbol,
          replacement = as.character(threshold) |>
            substring(first = 2) # TODO: fix for nonstandard thresholds
        ) |>
        as.numeric()
    }
  }
  return(output)
}
