test_that("basic case works", {
  check_date_range() |>
    expect_no_error() |>
    expect_no_warning()
  # TODO: check date range getss restricted and warnings/errors handled properly
})

test_that("chearacter inputs works", {
  # Test "now"
  check_date_range("now") |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range(c("now", "now")) |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range("now", "now") |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range(c("now", "now"), "now") |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range("now", c("now", "now")) |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range(c("now", "now"), c("now", "now")) |>
    expect_no_error() |>
    expect_no_warning()

  # Test "Y-m-d H"
  check_date_range("2020-01-01 00:00:00") |> # one day
    expect_no_error() |>
    expect_no_warning()
  check_date_range(c("2020-01-01 00:00:00", "2020-01-01 00:00:00")) |> # same day range
    expect_no_error() |>
    expect_no_warning()
  check_date_range(c("2020-01-01 00:00:00", "2020-05-01 00:00:00")) |> # longer range
    expect_no_error() |>
    expect_no_warning()
  check_date_range("2020-01-01 00:00:00", within = "2020-01-01 00:00:00") |> # same day, within same day
    expect_no_error() |>
    expect_no_warning()
  check_date_range(
    c("2020-01-01 00:00:00", "2020-05-01 00:00:00"), # range partially outside of within
    within = "2020-05-01 00:00:00"
  ) |>
    expect_warning() |>
    expect_no_error()
  check_date_range("2020-01-01 00:00:00", within = "2020-05-01 00:00:00") |> # range entirely outside of within
    expect_error()
  check_date_range(
    "2020-01-01 00:00:00", # one day, within range
    within = c("2020-01-01 00:00:00", "2020-05-01 00:00:00")
  ) |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range(
    c("2020-01-01 00:00:00", "2020-05-01 00:00:00"), # longer range, within range
    within = c("2020-01-01 00:00:00", "2020-05-01 00:00:00")
  ) |>
    expect_no_error() |>
    expect_no_warning()
})

test_that("date/datetime inputs work", {
  check_date_range(as.POSIXct("2020-01-01 00:00:00", tz = "UTC")) |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range(as.POSIXct(c("2020-01-01 00:00:00", "2020-05-01 00:00:00"), tz = "UTC")) |>
    expect_no_error() |>
    expect_no_warning()
})

test_that("NAs handled properly", {

  # NA in within
  check_date_range(within = c(NA, "now")) |>
    expect_no_error() |>
    expect_no_warning()
  check_date_range("2020-01-01 00:00:00", within = c(NA, "2020-05-01 00:00:00")) |>
    expect_no_error() |>
    expect_no_warning()
})

test_that("tz works as expected", {
  utc <- check_date_range(tz = "UTC")
  new_york <- check_date_range(tz = "America/New_York")

  expect_equal(attr(utc, "tzone"), "UTC")
  expect_equal(attr(new_york, "tzone"), "America/New_York")
})

test_that("now_time_step works as expected", {
  hourly <- check_date_range(now_time_step = "1 hour")
  daily <- check_date_range(now_time_step = "1 day")
  biminutely <- check_date_range(now_time_step = "2 mins")

  expect_equal(attr(hourly, "tzone"), "UTC")
  expect_equal(attr(daily, "tzone"), "UTC")
  expect_equal(attr(biminutely, "tzone"), "UTC")
  expect_equal(format(hourly[1], "%M:%S"), "00:00")
  expect_equal(format(daily[1], "%H:%M:%S"), "00:00:00")
  expect_equal(format(biminutely[1], "%S"), "00")
})
