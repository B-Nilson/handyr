#' Convert between common units
#'
#' @param x A numeric vector to convert
#' @param from A string indicating the current unit of `x`. Examples: "cm", "mm", "in", "ft", "m", "km", "mi", "yd", "nm", "mi", "mile", "km", "mi", "yd", "ft", "in", "mm", "cm"
#' @param to A string indicating the desired unit of `x`. Examples: "cm", "mm", "in", "ft", "m", "km", "mi", "yd", "nm", "mi", "mile", "km", "mi", "yd", "ft", "in", "mm", "cm"
#' @param ... Additional arguments passed on to [units::set_units()]
#' @description A wrapper around [units::set_units()] to convert between common units
#'  Run [units::valid_udunits()] for a list of valid units
#' @return A numeric vector of `x` converted to the desired unit
#' @export
#' @examples
#' convert_units(c(1, 2, 3), from = "cm", to = "km")
#' convert_units(c(1, 2, 3), from = "degF", to = "degC")
#' convert_units(c(1, 2, 3), from = "ft", to = "m")
#' convert_units(c(1, 2, 3), from = "oz", to = "L")
#' convert_units(c(1, 2, 3), from = "m/s", to = "km/h")
#' convert_units(c(1, 2, 3), from = "ug/m3", to = "g/km3")
convert_units <- function(x, from, to, ...) {
  rlang::check_installed("units")
  # handle inputs
  stopifnot(is.numeric(x), is.character(from), is.character(to))
  stopifnot(length(from) == 1, length(to) == 1)

  x |>
    units::set_units(from, mode = "standard", ...) |>
    units::set_units(to, mode = "standard", ...) |>
    as.numeric()
}
