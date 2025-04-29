test_that("basic case works", {
  cities <- data.frame(
    name = c("Nanaimo", "Port Moody", "Prince George"),
    x = c(-124.0531, -122.8519, -122.7949),
    y = c(49.1633, 49.2844, 53.8934)
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
