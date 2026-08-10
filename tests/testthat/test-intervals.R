test_that("able to make intervals", {
  date_range <- c("2025-01-01 00:00:00", "2025-01-12 00:00:00")
  expected <- lubridate::`%--%`(
    lubridate::as_datetime(date_range[1]),
    lubridate::as_datetime(date_range[2])
  )
  date_range |>
    lubridate::as_datetime() |>
    as_interval() |>
    expect_equal(expected)

  date_range |>
    as_interval() |>
    expect_equal(expected)
})

test_that("functions are vectorised", {
  date_ranges <- c(
    start = "2025-01-01 00:00:00",
    end = "2025-01-12 00:00:00"
  ) |>
    lapply(rep, times = 24) |>
    lapply(lubridate::as_datetime) |>
    as.data.frame()
  date_ranges$end <- date_ranges$end + lubridate::days(c(1:24))

  expected <- lubridate::`%--%`(date_ranges$start, date_ranges$end)

  date_ranges$start |>
    as_interval(start = _, end = date_ranges$end) |>
    expect_equal(expected)

  date_ranges |>
    as_interval() |>
    expect_equal(expected)

  date_ranges |>
    as_interval() |>
    seq(by = "1 hours") |>
    expect_snapshot()
})

test_that("able to make sequences from intervals", {
  date_range <- c("2025-01-01 00:00:00", "2025-01-12 00:00:00") |>
    lubridate::as_datetime()
  expected <- seq(date_range[1], date_range[2], by = "1 hours")
  date_range |>
    as_interval() |>
    seq(by = "1 hours") |>
    expect_equal(expected)
})

test_that("by = 'auto' works for sub-day intervals", {
  from <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
  # < 1 minute -> seconds
  expect_equal(
    lubridate::interval(from, from + 30) |> seq(),
    seq(from, from + 30, by = "1 secs")
  )
  # < 1 hour -> minutes
  expect_equal(
    lubridate::interval(from, from + 1800) |> seq(),
    seq(from, from + 1800, by = "1 mins")
  )
  # < 1 day -> hours
  expect_equal(
    lubridate::interval(from, from + 36000) |> seq(),
    seq(from, from + 36000, by = "1 hours")
  )
  # < 1 year -> days
  expect_equal(
    lubridate::interval(from, from + 7 * 86400) |> seq(),
    seq(from, from + 7 * 86400, by = "1 days")
  )
  # >= 1 year -> years
  expect_equal(
    lubridate::interval(from, from + 365 * 86400) |> seq(),
    seq(from, from + 365 * 86400, by = "1 years")
  )
})

test_that("by = 'auto' picks the coarser step at exact unit boundaries", {
  from <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
  expect_equal(
    lubridate::interval(from, from + 60) |> seq(),
    seq(from, from + 60, by = "1 mins")
  )
  expect_equal(
    lubridate::interval(from, from + 3600) |> seq(),
    seq(from, from + 3600, by = "1 hours")
  )
  expect_equal(
    lubridate::interval(from, from + 86400) |> seq(),
    seq(from, from + 86400, by = "1 days")
  )
  expect_equal(
    lubridate::interval(from, from + 31536000) |> seq(),
    seq(from, from + 31536000, by = "1 years")
  )
})

test_that("is_within works", {
  date_ranges <- c(
    start = "2025-01-01 00:00:00",
    end = "2025-01-12 00:00:00"
  ) |>
    lapply(rep, times = 24) |>
    lapply(lubridate::as_datetime) |>
    as.data.frame()
  date_ranges$end <- date_ranges$end + lubridate::days(c(1:24))

  date_ranges$end |>
    rev() |>
    is_within(interval = date_ranges[12, ]) |>
    expect_equal(rep(c(FALSE, TRUE), each = 12))
})
