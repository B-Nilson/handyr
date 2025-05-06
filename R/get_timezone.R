#' Lookup timezones of latitude/longitude pairs
#'
#' `get_timezone` uses the `lutz` package to lookup the timezone of locations from lat/lng coords. See [lutz::tz_lookup_coords] for more details.
#'
#' @param lng,lat Numeric longitude/latitude pairs (decimal degrees).
#' @param method either "accurate" or "fast",  indicating the lookup method to use.
#'   See [lutz::tz_lookup_coords] for more details.
#'   Set `.quiet` to `TRUE` to suppress warning produced when `method = "fast"`.
#' @param quiet A logical value indicating if messages and warnings should be suppressed.
#' @param ... Additional parameters to pass to [lutz::tz_lookup_coords] (currently unused)
#'
#' @return a character vector with the same length as `lat` and `lng` indicating each locations likely timezone.
#'  See [lutz::tz_lookup_coords] for more details.
#'
#' @export
#'
#' @examples
#' get_timezone(lng = -105.053144, lat = 69.116178, method = "accurate")
#' get_timezone(lng = c(-105.053144, -106.053144), lat = c(69.116178, 49.116178), method = "fast")
get_timezone <- function(lng, lat, method = "accurate", quiet = FALSE, ...) {
  # TODO: make this tidy friendly with{{ }} ?
  # TODO: leverage convert_coordinates to allow for x/y/crs inputs instead of lat/lng
  rlang::check_installed("lutz")
  # Handle inputs
  stopifnot(
    all(!is.na(lng)), all(!is.na(lat)),
    is.numeric(lng), is.numeric(lat),
    all(lng >= -180), all(lng <= 180),
    all(lat >= -90), all(lat <= 90),
    length(lng) > 0, length(lat) > 0,
    length(lng) == length(lat)
  )
  stopifnot(method %in% c("accurate", "fast"), length(method) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Lookup timezone(s)
  lutz::tz_lookup_coords(
    lat = lat,
    lon = lng,
    method = method,
    warn = !quiet,
    ...
  )
}
