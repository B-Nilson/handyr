#' Convert an sf object to a data.frame
#'
#' @description This function removes the geometry from an sf object, effectively converting it back to a data.frame.
#'
#' @param sf_obj An object of class `sf`, typically created by [sf::st_as_sf()]
#' @param keep_coords A logical value indicating if the coordinates should be kept when dropping the geometry
#'
#' @return A `data.frame` object without the geometry column.
#' @export
#'
#' @examples
#' cities <- data.frame(
#'   name = c("New York", "Los Angeles", "Chicago"),
#'   x = c(-74.0060, -118.2437, -87.6298),
#'   y = c(40.71278, 34.05224, 41.87811)
#' )
#' cities_sf <- cities |>
#'   sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
#' sf_as_df(cities_sf)
#' sf_as_df(cities_sf, keep_coords = TRUE)
sf_as_df <- function(sf_obj, keep_coords = FALSE) {
  rlang::check_installed("sf")
  stopifnot(inherits(sf_obj, "sf"))
  stopifnot(is.logical(keep_coords), length(keep_coords) == 1)

  if (keep_coords) {
    sf_obj <- extract_sf_coords(sf_obj)
  }
  sf::st_geometry(sf_obj) <- NULL
  return(sf_obj)
}
