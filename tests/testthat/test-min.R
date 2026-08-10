test_that("basic case works", {
  expect_length(
    min(1:5),
    n = 1
  )
  expect_equal(
    min(1:5),
    expected = 1
  )
  expect_length(
    min(NA_real_, NA_real_, NA_real_),
    n = 1
  )
  expect_equal(
    min(NA_real_, NA_real_, NA_real_),
    expected = NA_real_
  )
})

test_that("na.rm works", {
  expect_equal(
    min(NA_real_, NA_real_, NA_real_, na.rm = TRUE),
    expected = NA_real_
  )
  expect_equal(
    min(NA_real_, NA_real_, 1, na.rm = FALSE),
    expected = NA_real_
  )
  expect_equal(
    min(NA_real_, NA_real_, NA_real_, 1, na.rm = TRUE),
    expected = 1
  )
})

test_that("correct NA returned", {
  expect_equal(
    min(NA_real_, NA_real_, 1.1),
    expected = NA_real_
  )
  expect_equal(
    min(NA_integer_, NA_integer_, 1),
    expected = NA_integer_
  )
  expect_equal(
    min(NA_character_, NA_character_, "test"),
    expected = NA_character_
  )
})

test_that("empty/NULL input returns a single NA", {
  expect_length(min(), 1)
  expect_equal(min(), NA)
  expect_equal(min(NULL), NA)
  expect_equal(min(numeric(0), na.rm = TRUE), NA_real_)
})

test_that("dates are supported", {
  dates <- as.Date(c("2022-01-01", "2022-01-03"))
  expect_equal(
    min(dates),
    expected = as.Date("2022-01-01")
  )
  expect_equal(
    min(c(dates, NA), na.rm = TRUE),
    expected = as.Date("2022-01-01")
  )
  # All-NA dates return a Date NA, not a numeric NA
  expect_s3_class(
    min(as.Date(c(NA, NA)), na.rm = TRUE),
    class = "Date"
  )

  datetimes <- as.POSIXct(
    c("2022-01-01 00:00:00", "2022-01-03 00:00:00"),
    tz = "UTC"
  )
  expect_equal(
    min(datetimes),
    expected = as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
  )
  expect_s3_class(
    min(as.POSIXct(c(NA, NA), tz = "UTC"), na.rm = TRUE),
    class = "POSIXct"
  )
})
