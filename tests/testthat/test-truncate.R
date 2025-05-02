test_that("basic case works", {
  expect_equal(
    truncate(c(1.23, 4.56, 4, NA), digits = 1), 
    expected = c(1.2, 4.5, 4, NA)
  )
})

test_that("integers works", {
  expect_equal(
    truncate(c(1, 4, 4, NA), digits = 1), 
    expected = c(1, 4, 4, NA)
  )
})
