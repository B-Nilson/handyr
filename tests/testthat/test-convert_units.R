test_that("basic case works", {
  expect_equal(convert_units(c(1, 2, 3), from = "m", to = "km"), c(0.001, 0.002, 0.003))
})
