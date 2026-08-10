test_that("basic case works", {
  expect_equal(mode(1, 1, 1, 1, 2, 3), 1)
  expect_equal(mode(1, 2, 3, 1, 2, 4), 1)
  expect_equal(mode(1, 2, 3, NA, NA, NA), NA_real_)
  expect_equal(mode(1, 2, 3, 3, NA, NA, na.rm = TRUE), 3)
  expect_equal(mode(NA_real_, NA_real_), NA_real_)
})

test_that("dates are supported", {
  dates <- as.Date(c("2022-01-01", "2022-01-03", "2022-01-03"))
  expect_equal(
    mode(dates),
    expected = as.Date("2022-01-03")
  )
  # All-NA dates return a Date NA, not a numeric NA
  expect_s3_class(
    mode(as.Date(c(NA, NA)), na.rm = TRUE),
    class = "Date"
  )
})
