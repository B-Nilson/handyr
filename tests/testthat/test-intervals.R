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
    expect_equal(expected)
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
