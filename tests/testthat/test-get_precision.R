test_that("basic case works", {
  get_precision(1:5) |>
    expect_all_equal(0)
  get_precision(c(1.2, 1.23, 1.234)) |>
    expect_equal(c(1, 2, 3))
  get_precision(NA_real_) |>
    expect_equal(NA_integer_)
  get_precision(Inf) |>
    expect_equal(0)
})

test_that("tiny and huge numbers work", {
  get_precision(1e-7) |>
    expect_equal(7)
  get_precision(1e-16) |>
    expect_equal(16)
  get_precision(1e16) |>
    expect_equal(0)
  get_precision(1e300) |>
    expect_equal(0)
  get_precision(0.001) |>
    expect_equal(3)
  # full precision is retained for values with many decimals
  get_precision(1234.5678) |>
    expect_equal(4)
})
