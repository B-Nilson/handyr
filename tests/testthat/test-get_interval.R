test_that("basic case works", {
  expect_equal(
    get_interval(c(1, 3, 5:10)),
    1
  )
  expect_equal(
    get_interval(as.Date(c("2020-01-01", "2020-01-03", "2020-01-05", "2020-01-06"))),
    as.difftime(2, units = "days")
  )
  expect_equal(
    get_interval(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:02", "2020-01-01 00:00:04", "2020-01-01 00:00:05"))),
    as.difftime(2, units = "secs")
  )
})

test_that("most_common works", {
  expect_equal(
    get_interval(c(0, 3, 5, 7:10), most_common = FALSE),
    data.frame(interval = c(1, 2, 3), frequency = c(3, 2, 1))
  )
})

test_that("na.rm works", {
  expect_equal(
    get_interval(c(1, 3, 5:10, NA), na.rm = FALSE),
    1
  )
  expect_equal(
    get_interval(c(1, 3, 5:10, NA), na.rm = TRUE),
    1
  )
  expect_equal(
    get_interval(c(1, 3, NA, NA, NA, NA), na.rm = FALSE),
    NA_real_
  )
  expect_equal(
    get_interval(c(1, 3, NA, NA, NA, NA), na.rm = TRUE),
    2
  )
})

test_that("quiet works", {
  expect_invisible(expect_no_warning(expect_no_error(
    get_interval(c(1, 3, 5:10), quiet = TRUE)
  )))
})
