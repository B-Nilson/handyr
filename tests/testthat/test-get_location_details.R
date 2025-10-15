test_that("basic case works", {
  # Random locations from LLM
  latitudes <- c(49.1633, 53.8934, 43.6529, 49.116178, 56.0357)
  longitudes <- c(-124.0531, -122.7949, -79.3832, -122.8519, -121.9038)
  list(x = longitudes, y = latitudes) |>
    get_location_details() |>
    expect_snapshot()
})

test_that("many locations work", {
  skip("Takes a long time to run")
  monitor_locations <- readRDS(url(
    "https://aqmap.ca/aqmap/data/aqmap_most_recent_obs.Rds"
  )) |>
    head(100)

  details <- monitor_locations |>
    with(
      get_location_details(latitudes = lat, longitudes = lng)
    )
})
