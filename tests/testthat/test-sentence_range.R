test_that("basic case works", {
  expect_equal(
    sentence_range(c(1, 2, 3)),
    expected = "1-3"
  )
  expect_equal(
    sentence_range(c(1:3, 5:6, 8, 10:100)),
    expected = "1-3, 5, 6, 8 and 10-100"
  )
})

test_that("reverse works", {
  x <- c(1:3, 5:6, 8, 10:100)
  converted <- sentence_range(x)
  x |>
    expect_equal(
      sentence_range(converted, reverse = TRUE)
    )
})
