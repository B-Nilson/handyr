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
