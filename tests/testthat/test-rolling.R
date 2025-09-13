test_that("basic case works", {
  expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean)
  ))
})

test_that("FUN works", {
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling(mean) |> sum(na.rm = TRUE)
    )),
    expected = 9
  )
})

test_that(".width works", {
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling(mean, .width = 4) |> sum(na.rm = TRUE)
    )),
    expected = 6
  )
})

test_that(".direction works", {
  expect_no_warning(expect_no_error(
    (1:5) |> rolling(mean, .direction = "forward")
  ))
})

test_that(".fill works", {
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling(mean, .fill = -1) |> sum()
    )),
    expected = 7
  )
})

test_that(".min_length works", {
  expect_equal(
    expect_no_warning(expect_no_error(
      c(1:3, NA_real_) |> rolling(mean, .min_length = 4) |> sum()
    )),
    expected = NA_real_
  )
})

test_that("vectorized FUNs work and are faster", {
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling("sum", .fill = NULL)
    )),
    expected = c(1, 3, 6, 9, 12)
  )
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling("mean", .fill = NULL)
    )),
    expected = c(1, 1.5, 2, 3, 4)
  )
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling("min", .fill = NULL)
    )),
    expected = c(1, 1, 1, 2, 3)
  )
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling("max", .fill = NULL)
    )),
    expected = 1:5
  )
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling("quantile", .fill = NULL, probs = 0.5)
    )),
    expected = c(1, 1.5, 2, 3, 4)
  )
  expect_equal(
    expect_no_warning(expect_no_error(
      (1:5) |> rolling("median", .fill = NULL)
    )),
    expected = c(1, 1.5, 2, 3, 4)
  )

  test_vals <- 1:50000
  normal_speed <- test_vals |> rolling(sum) |> system.time()
  vectorized_speed <- test_vals |> rolling("sum") |> system.time()
  expect_true(vectorized_speed[["elapsed"]] < normal_speed[["elapsed"]])

  normal_speed <- test_vals |> rolling(mean) |> system.time()
  vectorized_speed <- test_vals |> rolling("mean") |> system.time()
  expect_true(vectorized_speed[["elapsed"]] < normal_speed[["elapsed"]])
  
  normal_speed <- test_vals |> rolling(max) |> system.time()
  vectorized_speed <- test_vals |> rolling("max") |> system.time()
  expect_true(vectorized_speed[["elapsed"]] < normal_speed[["elapsed"]])
  
  normal_speed <- test_vals |> rolling(min) |> system.time()
  vectorized_speed <- test_vals |> rolling("min") |> system.time()
  expect_true(vectorized_speed[["elapsed"]] < normal_speed[["elapsed"]])

  normal_speed <- test_vals |> rolling(quantile, probs = 0.5) |> system.time()
  vectorized_speed <- test_vals |> rolling("quantile", probs = 0.5) |> system.time()
  expect_true(vectorized_speed[["elapsed"]] < normal_speed[["elapsed"]])

})
