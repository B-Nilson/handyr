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
#'   name = c("Nanaimo", "Port Moody", "Prince George"),
#'   x = c(-124.0531, -122.8519, -122.7949),
#'   y = c(49.1633, 49.2844, 53.8934)
#' )
#' cities_sf <- cities |>
#'   sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
#' sf_as_df(cities_sf)
#' sf_as_df(cities_sf, keep_coords = TRUE)
sf_as_df <- function(sf_obj, keep_coords = FALSE) {
  rlang::check_installed("sf")
  # Handle inputs
  stopifnot(inherits(sf_obj, "sf"))
  stopifnot(is.logical(keep_coords), length(keep_coords) == 1)

  # Extract coords/crs if desired
  if (keep_coords) {
    sf_obj <- sf_obj |> extract_sf_coords()
  }
  
  # Convert sf -> data.frame cleanly
  sf::st_geometry(sf_obj) <- NULL
  return(sf_obj)
}
