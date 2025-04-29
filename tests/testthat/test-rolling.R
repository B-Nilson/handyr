test_that("basic case works", {
  expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean)
  ))
})

test_that("FUN works", {
  expect_equal(expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean) |> sum(na.rm = TRUE)
  )), expected = 27)
})

test_that(".width works", {
  expect_equal(expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean, .width = 4) |> sum(na.rm = TRUE)
  )), expected = 6)
})

test_that(".direction works", {
  expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean, .direction = "forward")
  ))
})

test_that(".fill works", {
  expect_equal(expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean, .fill = -1) |> sum()
  )), expected = 7)
})

test_that(".min_length works", {
  expect_equal(expect_no_warning(expect_no_error(
    c(1:3, NA_real_) |> rolling(mean, .min_length = 4) |> sum()
  )), expected = NA_real_)
})
