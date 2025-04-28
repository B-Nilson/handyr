test_that("basic case works", {
  cities <- data.frame(
    name = c("New York", "Los Angeles", "Chicago"),
    x = c(-74.0060, -118.2437, -87.6298),
    y = c(40.71278, 34.05224, 41.87811)
  )
  cities_sf <- cities |>
    sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
  expect_equal(
    sf_as_df(cities_sf),
    cities[1]
  )
  expect_equal(
    sf_as_df(cities_sf, keep_coords = TRUE) |> dplyr::select(-crs),
    cities
  )
})
