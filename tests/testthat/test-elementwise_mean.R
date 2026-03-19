test_that("basic case works", {
  result <- elementwise_mean(c(1, 2, 3), c(1, 2, 3)) |> 
    expect_silent()
  expect_equal(result, expected = c(1, 2, 3))
  result <- elementwise_mean(c(1, 2, 3), c(6, 2, 0)) |> 
    expect_silent()
  expect_equal(result, expected = c(3.5, 2, 1.5))

  result <- elementwise_mean(c(NA, 2, 3), c(6, 2, 0)) |> 
    expect_silent()
  expect_equal(result, expected = c(NA, 2, 1.5))

  result <- elementwise_mean(c(NA, 2, 3), c(6, 2, 0), na.rm = TRUE) |> 
    expect_silent()
  expect_equal(result, expected = c(6, 2, 1.5))
})
