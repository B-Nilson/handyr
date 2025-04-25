test_that("basic case works", {
  expect_no_warning(expect_no_error(
    swap(c(1:2, NA), what = NA, with = -1)
  ))
})
