test_that("basic case works", {
  test_list <- lapply(1:3, \(x) {
    data.frame(x = x, y = x^2) |>
      stats::setNames(c("x", paste0("y", x)))
  })
  expected <- data.frame(
    x = 1:3,
    y1 = c(1, NA, NA),
    y2 = c(NA, 4, NA),
    y3 = c(NA, NA, 9)
  )
  expect_equal(
    join_list(test_list, by = "x"),
    expected
  )
})
