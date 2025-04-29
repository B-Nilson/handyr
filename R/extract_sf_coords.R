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
#'   x = c(-74.0060, -118.2437, -87.6298),
#'   y = c(40.71278, 34.05224, 41.87811)
#' )
#' cities_sf <- cities |>
#'   sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
#' cities_sf |>
#'   extract_sf_coords()
#' @export
extract_sf_coords <- function(sf_obj, keep_sf = TRUE) {
  rlang::check_installed("sf")
  # Handle inputs
  stopifnot(inherits(sf_obj, "sf"))
  stopifnot(is.logical(keep_sf), length(keep_sf) == 1)

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
  }
  sf_as_df(out, keep_coords = FALSE) # coords already are extracted
}
