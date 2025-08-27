test_that("basic case works", {
  expect_equal(
    get_timezone(
      lng = -122.084051,
      lat = 37.385348,
      method = "fast",
      quiet = TRUE
    ),
    "America/Los_Angeles"
  )
  expect_equal(
    get_timezone(
      lng = -122.084051,
      lat = 37.385348,
      method = "accurate",
      quiet = TRUE
    ),
    "America/Los_Angeles"
  )
})
