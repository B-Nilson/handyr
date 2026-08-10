test_that("basic case works", {
  expect_length(
    max(1:5),
    n = 1
  )
  expect_equal(
    max(1:5),
    expected = 5
  )
  expect_length(
    max(NA_real_, NA_real_, NA_real_),
    n = 1
  )
  expect_equal(
    max(NA_real_, NA_real_, NA_real_),
    expected = NA_real_
  )
})

test_that("na.rm works", {
  expect_equal(
    max(NA_real_, NA_real_, NA_real_, na.rm = TRUE),
    expected = NA_real_
  )
  expect_equal(
    max(NA_real_, NA_real_, 1, na.rm = FALSE),
    expected = NA_real_
  )
  expect_equal(
    max(NA_real_, NA_real_, NA_real_, 1, na.rm = TRUE),
    expected = 1
  )
})

test_that("correct NA returned", {
  expect_equal(
    max(NA_real_, NA_real_, 1.1),
    expected = NA_real_
  )
  expect_equal(
    max(NA_integer_, NA_integer_, 1),
    expected = NA_integer_
  )
  expect_equal(
    max(NA_character_, NA_character_, "test"),
    expected = NA_character_
  )
})

test_that("empty/NULL input returns a single NA", {
  expect_length(max(), 1)
  expect_equal(max(), NA)
  expect_equal(max(NULL), NA)
  expect_equal(max(numeric(0), na.rm = TRUE), NA_real_)
})

test_that("dates are supported", {
  dates <- as.Date(c("2022-01-01", "2022-01-03"))
  expect_equal(
    max(dates),
    expected = as.Date("2022-01-03")
  )
  expect_equal(
    max(c(dates, NA), na.rm = TRUE),
    expected = as.Date("2022-01-03")
  )
  # All-NA dates return a Date NA, not a numeric NA
  expect_s3_class(
    max(as.Date(c(NA, NA)), na.rm = TRUE),
    class = "Date"
  )

  datetimes <- as.POSIXct(
    c("2022-01-01 00:00:00", "2022-01-03 00:00:00"),
    tz = "UTC"
  )
  expect_equal(
    max(datetimes),
    expected = as.POSIXct("2022-01-03 00:00:00", tz = "UTC")
  )
  expect_s3_class(
    max(as.POSIXct(c(NA, NA), tz = "UTC"), na.rm = TRUE),
    class = "POSIXct"
  )
})
