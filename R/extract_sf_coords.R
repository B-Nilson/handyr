#' Add sf coordinates and crs as columns
#'
#' `extract_sf_coords` adds the x and y coordinates and the crs as additional columns to an `sf` object, with the option to convert it to a `data.frame`.
#'
#' @param sf_obj An object of class `sf`, typically created by [sf::st_as_sf()]
#' @param keep_sf A logical value indicating if the return object should remain an `sf` object or be converted to a `data.frame`
#' @param add_crs A logical value indicating if the crs should be added as a column
#'
#' @return
#'  If `keep_sf` is:
#'   * `TRUE` `sf_obj` with two additional numeric columns (`x` and `y`) and a character column (`crs`).
#'   * `FALSE`: same as above, but converted to a `data.frame`
#'
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
#' cities_sf |>
#'   extract_sf_coords()
extract_sf_coords <- function(sf_obj, keep_sf = TRUE, add_crs = TRUE) {
  rlang::check_installed("sf")
  # Handle inputs
  stopifnot(inherits(sf_obj, "sf"))
  stopifnot(is.logical(keep_sf), length(keep_sf) == 1)
  stopifnot(is.logical(add_crs), length(add_crs) == 1)

  # Extract coords/crs
  coords <- sf_obj |>
    sf::st_coordinates()
  crs <- sf::st_crs(sf_obj$geometry)$input
  # Add columns to input
  sf_obj$x <- coords[, 1]
  sf_obj$y <- coords[, 2]
  if (add_crs) {
    sf_obj$crs <- crs
  }

  # Return if sf typing desired
  if (keep_sf) {
    return(sf_obj)
  }
  # Return as df if not
  sf_obj |>
    sf_as_df(keep_coords = FALSE) # (FALSE because coords already are extracted)
}
