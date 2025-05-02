test_that("basic case works", {
  # numeric
  expect_equal(
    expect_no_warning(
      get_interval(c(1, 3, 5:10, 1.1, 1.2, 1.3))
    ),
    expected = 1
  )
  expect_equal(
    expect_no_warning(
      get_interval(c(1, 1, 1))
    ),
    expected = 0
  )
  expect_equal(
    expect_no_warning(
      get_interval(c(1, 2, 3))
    ),
    expected = 1
  )
  expect_equal(
    expect_no_warning(
      get_interval(c(1.1, 1.2, 1.3))
    ),
    expected = 0.1
  )
  # Date
  expect_equal(
    get_interval(as.Date(c("2020-01-01", "2020-01-03", "2020-01-05", "2020-01-06"))),
    expected = as.difftime(2, units = "days")
  )
  expect_equal(
    get_interval(as.Date(c("2020-01-01", "2020-01-03", "2020-01-05"))),
    expected = as.difftime(2, units = "days")
  )
  expect_equal(
    get_interval(as.Date(c("2020-01-01", "2020-01-01", "2020-01-01"))),
    expected = as.difftime(0, units = "days")
  )
  # Datetime
  expect_equal(
    get_interval(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:02", "2020-01-01 00:00:04", "2020-01-01 00:00:05"))),
    expected = as.difftime(2, units = "secs")
  )
  expect_equal(
    get_interval(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 01:00:00", "2020-01-01 02:00:00"))),
    expected = as.difftime(1, units = "hours")
  )
  expect_equal(
    get_interval(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:00", "2020-01-01 00:00:00"))),
    expected = as.difftime(0, units = "secs")
  )
})

test_that("most_common works", {
  expect_equal(
    get_interval(c(0, 3, 5, 7:10), most_common = FALSE),
    expected = data.frame(interval = c(1, 2, 3), frequency = c(3, 2, 1))
  )
  expect_equal(
    get_interval(c(0, 3, 5, 7:10), most_common = FALSE, quiet = TRUE),
    expected = data.frame(interval = c(1, 2, 3), frequency = c(3, 2, 1))
  )
})

test_that("na.rm works", {
  expect_equal(
    get_interval(c(1, 3, 5:10, NA), na.rm = FALSE),
    expected = 1
  )
  expect_equal(
    get_interval(c(1, 3, 5:10, NA), na.rm = TRUE),
    expected = 1
  )
  expect_equal(
    get_interval(c(1, 3, NA, NA, NA, NA), na.rm = FALSE),
    expected = NA_real_
  )
  expect_equal(
    get_interval(c(1, 3, NA, NA, NA, NA), na.rm = TRUE),
    expected = 2
  )
})

test_that("quiet works", {
  expect_invisible(
    get_interval(c(1, 3, 5:10), quiet = TRUE)
  )
})

test_that("works when not needed", {
  expect_equal(
    get_interval(c(NA_real_, NA_real_, NA_real_), na.rm = FALSE),
    expected = NA_real_
  )
  expect_equal(
    get_interval(numeric(0), na.rm = FALSE),
    expected = NA_real_
  )
})

test_that("warning if 2+ equal top intervals", {
  expect_warning(
    expect_equal(
      get_interval(c(1:3, seq(4, 10, by = 2))),
      expected = 1
    )
  )
})
