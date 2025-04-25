test_that("basic case works", {
  expect_no_warning(expect_no_error(
    clamp(1:5, range = c(2, 4))
  ))
  expect_no_warning(expect_no_error(
    clamp(1:5, range = c(-1, 8))
  ))
})
