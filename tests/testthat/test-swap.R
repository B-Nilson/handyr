test_that("basic case works", {
  expect_no_warning(expect_no_error(
    swap(c(1:2, NA), what = NA, with = -1)
  ))
})

test_that("more than 1 with works", {
  expect_no_warning(expect_no_error(
    swap(c(1:2, NA), what = c(NA, 2), with = -1)
  ))
})

test_that("logical what works", {
  x <- 1:10
  swap(x, what = x < 5, with = -1) |>
    expect_equal(c(-1, -1, -1, -1, 5, 6, 7, 8, 9, 10))
})

test_that("more than 1 with works", {
  x <- 1:10
  y <- 10:1
  swap(x, what = x < 5, with = y) |>
    expect_equal(c(10, 9, 8, 7, 5, 6, 7, 8, 9, 10))
})
