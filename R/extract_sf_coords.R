#' Extract coordinates from an sf object and add them as columns
#'
#' @param sf_obj An object of class `sf`, typically created by [sf::st_as_sf()]
#' @param keep_sf A logical value indicating if the return object should remain an `sf` object or be converted to a `data.frame`
#'
#' @return An `sf` object with two additional columns: `lng` and `lat` for longitude and latitude respectively.
#'
#' @examples
#'
#' cities <- data.frame(
#'   name = c("New York", "Los Angeles", "Chicago"),
#'   lat = c(40.71278, 34.05224, 41.87811),
#'   lon = c(-74.0060, -118.2437, -87.6298)
#' )
#' cities_sf <- cities |> 
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' cities_sf |>
#'   extract_sf_coords()
#' @export
extract_sf_coords <- function(sf_obj, keep_sf = TRUE) {
  rlang::check_installed("sf")

  coords <- sf_obj |>
    sf::st_coordinates()

  out <- sf_obj |>
    dplyr::mutate(
      x = coords[, 1],
      y = coords[, 2],
      crs = sf::st_crs(.data$geometry)$input
    )
  if (keep_sf) {
    return(out)
  } else {
    return(sf_as_df(out))
  }
}