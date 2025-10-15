#' Get a communities within a region from OpenStreetMap
#'
#' @param bbox Usually the name of an area (ie. "Fort St. John, BC").
#'   See [osmdata::getbb()] for more information.
#' @param types A character vector of types of communities to search for. Default is c("city", "town", "village", "hamlet"). See \href{https://wiki.openstreetmap.org/wiki/Key:place}{https://wiki.openstreetmap.org/wiki/Key:place}.
#' @param timeout The time in seconds to wait for a response from the OSM API. Default is 600 seconds.
#' @param quiet A logical value indicating whether non-critical warnings/messages should be suppressed. Default is TRUE.
#'
#' @return An `sf` data frame containing the communities in the bounding box
#'  with columns "osm_id", "type" (community type), "name", "population"/"population_source" (if available), and "geometry".
#'
#' @export
#'
#' @examples
#' get_communities("Fort St. John, BC")
#'
#' get_communities("Yukon, Canada")
get_communities <- function(
  bbox,
  types = c("city", "town", "village", "hamlet"),
  timeout = 600,
  quiet = TRUE
) {
  rlang::check_installed("osmdata")
  rlang::check_installed("sf")

  # Constants
  desired_cols <- c(
    "osm_id",
    type = "place",
    "name",
    "population",
    # Converted to population_source
    "population:date",
    "source:population",
    "source:population:date"
  )

  # Convert area name to bounding box
  if (is.character(bbox)) {
    bbox <- osmdata::getbb(bbox, limit = 1, format_out = "polygon")
  }

  # Get OSM data for select place types within bbox
  osm_results <- bbox |>
    osmdata::opq(timeout = timeout) |>
    osmdata::add_osm_feature(
      key = "place",
      value = types
    ) |>
    osmdata::osmdata_sf(quiet = quiet) |>
    osmdata::unique_osmdata() |>
    osmdata::trim_osmdata(bb_poly = bbox)

  # Reformat point data
  communities <- osm_results$osm_points |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    make_osm_population_source() |>
    dplyr::filter(stats::complete.cases(.data$name))

  if ("population" %in% names(communities)) {
    communities <- communities |>
      dplyr::mutate(population = as.numeric(.data$population)) |>
      dplyr::filter(is.na(.data$population) | .data$population > 0) |>
      dplyr::arrange(.data$population)
  }
  return(communities)
}

# Convert population:date, source:population, and source:population:date to population_source
make_osm_population_source <- function(osm_results) {
  # Replicate population:date if source:population:date is missing and vice versa
  if (
    "source:population:date" %in%
      names(osm_results) &
      !"population:date" %in% names(osm_results)
  ) {
    osm_results$`population:date` <- osm_results$`source:population:date`
  } else if (
    "population:date" %in%
      names(osm_results) &
      !"source:population:date" %in% names(osm_results)
  ) {
    osm_results$`source:population:date` <- osm_results$`population:date`
  }

  # Handle "source:population" missing when date provided
  if (
    !"source:population" %in% names(osm_results) &
      "population:date" %in%
        names(osm_results)
  ) {
    osm_results$`source:population` <- NA
  }

  # Handle not being able to build population_source
  if (
    !all(c("source:population:date", "population:date") %in% names(osm_results))
  ) {
    return(osm_results)
  }
  osm_results |>
    dplyr::mutate(
      population_source = paste0(
        .data$`source:population` |> swap(NA, with = "Unknown"),
        " (Date: ",
        ifelse(
          is.na(.data$`source:population:date`),
          .data$`population:date`,
          .data$`source:population:date`
        ) |>
          swap(NA, with = "Unknown"),
        ")"
      ) |>
        swap("Unknown (Date: Unknown)", with = NA)
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "source:population",
        "source:population:date",
        "population:date"
      ))
    )
}
