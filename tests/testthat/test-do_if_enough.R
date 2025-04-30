test_that("basic case works", {
  values <- c(1:2, NA, NA)
  expect_equal(
    values |> do_if_enough(mean, .min_non_na = 3), 
    expected = NA
  )
  expect_equal(
    values |> do_if_enough(mean, .min_non_na = 2), 
    expected = 1.5
  )
})
