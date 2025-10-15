get_location_details <- function(
  coordinates,
  quiet = FALSE
) {
  stopifnot(
    is.list(coordinates) & identical(sort(names(coordinates)), c("x", "y"))
  )
  stopifnot(length(coordinates$x) == length(coordinates$y))

  rlang::check_installed("tidygeocoder")

  desired_cols <- c(
    "lat",
    lng = "long",
    country = "Country",
    region = "Region",
    region_abbr = "RegionAbbr",
    subregion = "Subregion",
    city = "City",
    neighborhood = "Neighborhood",
    address = "Address"
  )

  coordinates$y |>
    tidygeocoder::reverse_geo(
      long = coordinates$x,
      method = "arcgis",
      progress_bar = !quiet,
      full_results = TRUE
    ) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      \(x) x |> swap("", with = NA)
    ))
}
