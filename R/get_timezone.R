#' Wrapper for looking up timezone of locations from lat/lng coords using the `lutz` package
#'
#' @param lng Vector of numeric values indicating location longitudes (decimal degrees) to lookup.
#' @param lat Vector of numeric values indicating location latitudes (decimal degrees) to lookup.
#' @param method A single character value indicating the lookup method to use (either "accurate" or "fast"). See [lutz::tz_lookup_coords] for more details.
#' @param ... (Optional) Additional paramaters to pass to [lutz::tz_lookup_coords]
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
#' get_timezone(-105.053144, 69.116178, method = "fast")
get_timezone <- function(lng, lat, method = "accurate", ...) {
  rlang::check_installed("lutz")
  # Handle inputs
  stopifnot(is.numeric(lng), is.numeric(lat))
  stopifnot(all(lng >= -180), all(lng <= 180), all(lat >= -90), all(lat <= 90))
  stopifnot(length(lng) == length(lat), length(lng) > 0)
  stopifnot(method %in% c("accurate", "fast"), length(method) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  lutz::tz_lookup_coords(
    lat = lat,
    lon = lng,
    method = method, ...
  )
}