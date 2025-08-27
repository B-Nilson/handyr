#' Convert between common units
#'
#' `convert_units` is a wrapper around [units::set_units()] to switch between common measurement units.
#'
#' @param x A numeric vector.
#' @param from,to A string indicating the current/desired unit of `x`.
#'  Run [units::valid_udunits()] for a list of valid units for your system.
#' @param keep_units A logical value indicating whether to keep the units after conversion (default is `FALSE`).
#' @param ... Additional arguments passed on to [units::set_units()]
#'
#' @return A numeric vector with the same length/type as `x`.
#' If `keep_units = TRUE`, the units attribute is preserved
#'
#' @export
#'
#' @examples
#' convert_units(c(1, 2, 3), from = "cm", to = "km")
#' convert_units(c(1, 2, 3), from = "degF", to = "degC")
#' convert_units(c(1, 2, 3), from = "ft", to = "m")
#' convert_units(c(1, 2, 3), from = "oz", to = "L")
#' convert_units(c(1, 2, 3), from = "m/s", to = "km/h")
#' convert_units(c(1, 2, 3), from = "ug/m3", to = "mg/km3")
convert_units <- function(x, from, to, keep_units = FALSE, ...) {
  rlang::check_installed("units")
  # handle inputs
  stopifnot(is.numeric(x), is.character(from), is.character(to))
  stopifnot(length(from) == 1, length(to) == 1)

  # convert units
  converted <- x |>
    units::set_units(from, mode = "standard", ...) |>
    units::set_units(to, mode = "standard", ...)
  # drop units if desired
  if (!keep_units) {
    converted <- converted |> as.numeric()
  }

  return(converted)
}
