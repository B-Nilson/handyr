#' Get timezones of latitude/longitude pairs
#'
#' @param lng Vector of numeric values indicating location longitudes (decimal degrees) to lookup.
#' @param lat Vector of numeric values indicating location latitudes (decimal degrees) to lookup.
#' @param method A single character value indicating the lookup method to use (either "accurate" or "fast"). See [lutz::tz_lookup_coords] for more details.
#' @param quiet A single logical value indicating if messages and warnings should be suppressed.
#' @param ... Additional paramaters to pass to [lutz::tz_lookup_coords] (currently unused)
#'
#' @description
#' `get_timezone` uses the `lutz` package to lookup the timezone of locations from lat/lng coords. See [lutz::tz_lookup_coords] for more details.
#'
#' @family Utilities
#'
#' @return a character vector with the same length as `lat` and `lng` indicating the locations likely timezone.
#' @export
#'
#' @examples
#' get_timezone(lng = -105.053144, lat = 69.116178, method = "accurate")
#' get_timezone(lng = c(-105.053144, -106.053144), lat = c(69.116178, 49.116178), method = "fast")
get_timezone <- function(lng, lat, method = "accurate", quiet = FALSE, ...) {
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
