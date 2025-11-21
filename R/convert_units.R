#' Convert between common units
#'
#' `convert_units` is a wrapper around [units::set_units()] to switch between common measurement units.
#'
#' @param x A numeric vector.
#' @param from,to A string indicating the current/desired unit of `x`.
#'  Run [units::valid_udunits()] for a list of valid units for your system.
#'  If `from` is `NULL` (default), tries to get units from `x` and fails if `x` does not have a `units` attribute.
#'  If `to` is `NULL`, [units::convert_to_base()] is used to convert to pre-defined base units for the `from` unit.
#' @param keep_units A logical value (or `NULL`) indicating whether to keep the units after conversion.
#'   Default is `NULL`, which is `TRUE` if `x` has a `units` attribute, `FALSE` otherwise.
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
#' 1:5 |> units::set_units("cm") |> convert_units(to = "km")
#' 1:5 |> units::set_units("cm") |> convert_units()
convert_units <- function(x, from = NULL, to = NULL, keep_units = NULL, ...) {
  rlang::check_installed("units")
  # handle inputs
  stopifnot(
    is.numeric(x),
    is.character(from) | is.null(from),
    is.character(to) | is.null(to)
  )
  stopifnot(length(from) == 1 | is.null(from), length(to) == 1 | is.null(to))
  has_units <- !is.null(attr(x, "units"))
  if (is.null(from)) {
    if (!has_units) {
      stop("If `from` is NULL, `x` must have a `units` attribute")
    }
    from <- attr(x, "units")
  }
  if (is.null(to)) {
    to <- 0 |>
      units::set_units(from, mode = "standard") |>
      units::convert_to_base() |>
      attr("units")
  }
  if (is.null(keep_units)) {
    keep_units <- has_units
  }

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
