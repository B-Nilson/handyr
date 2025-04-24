test_that("basic case works", {
  values <- c(1:2, NA, NA)
  expect_equal(expect_no_warning(expect_no_error(
    values |> do_if_enough(mean, .min_length = 3)
  )), NA)
})
