get_location_details <- function(latitudes, longitudes) {
  stopifnot(is.numeric(latitudes))
  stopifnot(is.numeric(longitudes))
  stopifnot(length(latitudes) == length(longitudes))
  stopifnot(all(!is.na(latitudes) & !is.na(longitudes)))
  stopifnot(all(latitudes >= -90) || all(latitudes <= 90))
  stopifnot(all(longitudes >= -180) || all(longitudes <= 180))

  rlang::check_installed("tidygeocoder")
  desired_cols <- c(
    "lat",
    lng = "long",
    "country",
    "state",
    "state_district",
    "county",
    "city",
    "town",
    "village",
    "municipality",
    "residential",
    "hamlet",
    "island",
    "quarter",
    "suburb",
    "borough",
    "neighbourhood",
    "city_block",
    "road",
    "house_number",
    "amenity",
    "building",
    "address",
    "postcode"
  )
  latitudes |>
    tidygeocoder::reverse_geo(
      long = longitudes,
      method = "osm",
      limit = NULL,
      full_results = TRUE
    ) #|>
    #dplyr::select(dplyr::any_of(desired_cols))
}

# Chooses the most likely community name based on location details
# (sometimes city, or town, or ...)
# TODO: expand on and test this
determine_likely_community <- function(location_details) {
  location_details |>
    dplyr::mutate(
      nearest_community = dplyr::case_when(
        !is.na(.data$city) ~ .data$city,
        !is.na(.data$town) ~ .data$town,
        !is.na(.data$village) ~ .data$village,
        !is.na(.data$municipality) ~ .data$municipality,
        !is.na(.data$hamlet) ~ .data$hamlet,
        !is.na(.data$quarter) ~ .data$quarter,
        !is.na(.data$suburb) ~ .data$suburb,
        TRUE ~ NA_character_
      )
    ) |> 
    dplyr::filter(is.na(nearest_community))
}
  
  
  for (priority in priorities) {
    if (!is.na(test[[priority]])) {
      return(test[[priority]])
    }
  }
  return(NA)
}