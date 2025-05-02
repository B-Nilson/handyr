test_that("basic case works", {
  expect_equal(
    clamp(1:5, range = c(2, 4)),
    expected = c(2, 2, 3, 4, 4)
  )
})

test_that("NA range values work", {
  expect_equal(
    clamp(1:5, range = c(NA, NA)), 
    expected = 1:5
  )
  expect_equal(
    clamp(1:5, range = c(NA, 4)), 
    expected = c(1:4, 4)
  )
  expect_equal(
    clamp(1:5, range = c(2, NA)), 
    expected = c(2, 2:5)
  )
})
