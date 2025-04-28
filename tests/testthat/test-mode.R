test_that("basic case works", {
  expect_equal(mode(1, 1, 1, 1, 2, 3), 1)
  expect_equal(mode(1, 2, 3, 1, 2, 4), 1)
  expect_equal(mode(1, 2, 3, NA, NA, NA), NA_real_)
  expect_equal(mode(1, 2, 3, 3, NA, NA, na.rm = TRUE), 3)
  expect_equal(mode(NA_real_, NA_real_), NA_real_)
})
