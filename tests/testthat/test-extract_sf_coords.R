test_that("basic case works", {
  cities <- data.frame(
    name = c("Nanaimo", "Port Moody", "Prince George"),
    x = c(-124.0531, -122.8519, -122.7949),
    y = c(49.1633, 49.2844, 53.8934)
  )
  cities_sf <- cities |>
    sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
  expect_no_warning(expect_no_error(
    cities_sf |>
      extract_sf_coords()
  ))
})

test_that("keep_sf works", {
  cities <- data.frame(
    name = c("Nanaimo", "Port Moody", "Prince George"),
    x = c(-124.0531, -122.8519, -122.7949),
    y = c(49.1633, 49.2844, 53.8934)
  )
  cities_sf <- cities |>
    sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
  out_df <- extract_sf_coords(cities_sf, keep_sf = FALSE)
  expect_equal(
    out_df |> dplyr::select(-crs),
    cities
  )
})
