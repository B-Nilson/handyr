test_that("basic case works", {
  expect_equal(
    clamp(1:5, range = c(2, 4)),
    expected = c(2, 2, 3, 4, 4)
  )
})

test_that("NA range values work", {
  expect_equal(
    clamp(1:5, range = c(NA, NA)),
    expected = 1:5
  )
  expect_equal(
    clamp(1:5, range = c(NA, 4)),
    expected = c(1:4, 4)
  )
  expect_equal(
    clamp(1:5, range = c(2, NA)),
    expected = c(2, 2:5)
  )
})

test_that("dates/times work", {
  dates <- seq(as.Date("2022-01-01"), as.Date("2022-01-05"))
  clamp_to <- c(as.Date("2022-01-03"), as.Date("2022-01-04"))
  expected <- dates
  expected[dates < clamp_to[1]] <- clamp_to[1]
  expected[dates > clamp_to[2]] <- clamp_to[2]
  expect_equal(
    clamp(dates, range = clamp_to),
    expected = c(clamp_to[1], clamp_to[1], dates[3:4], clamp_to[2])
  )

  datetimes <- seq(
    as.POSIXct("2022-01-01 00:00:00"),
    as.POSIXct("2022-01-05 00:00:00"),
    by = "12 hours"
  )
  clamp_to <- c(
    as.POSIXct("2022-01-03 00:00:00"),
    as.POSIXct("2022-01-04 00:00:00")
  )
  expected <- datetimes
  expected[datetimes < clamp_to[1]] <- clamp_to[1]
  expected[datetimes > clamp_to[2]] <- clamp_to[2]
  expect_equal(
    clamp(datetimes, range = clamp_to),
    expected = expected
  )
})

test_that("mixed types are rejected", {
  dates <- as.Date("2022-01-01")
  datetimes <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
  expect_error(
    clamp(dates, range = rep(datetimes, 2)),
    regexp = "same type"
  )
  expect_error(
    clamp(datetimes, range = rep(dates, 2)),
    regexp = "same type"
  )
  expect_error(
    clamp(1:5, range = rep(dates, 2)),
    regexp = "same type"
  )
  expect_error(
    clamp(dates, range = c(1, 2)),
    regexp = "same type"
  )
  # Unsupported range types are rejected too
  expect_error(
    clamp(1:5, range = c("2", "4")),
    regexp = "numeric, Date, POSIXct"
  )
})

test_that("same-type partially-NA ranges still work", {
  dates <- seq(as.Date("2022-01-01"), as.Date("2022-01-05"))
  expect_equal(
    clamp(dates, range = c(as.Date("2022-01-03"), NA)),
    expected = c(
      rep(as.Date("2022-01-03"), 3),
      as.Date("2022-01-04"),
      as.Date("2022-01-05")
    )
  )
  expect_equal(
    clamp(dates, range = c(NA, NA)),
    expected = dates
  )
})
