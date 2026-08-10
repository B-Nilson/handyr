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
  vectorized_speed <- test_vals |>
    rolling("quantile", probs = 0.5) |>
    system.time()
  expect_true(vectorized_speed[["elapsed"]] < normal_speed[["elapsed"]])
})

test_that("vectorized max/min respect .min_non_na", {
  x <- c(1, NA, NA, 2, 3, 4)
  expect_equal(
    x |> rolling("max", .width = 3, .min_non_na = 2, .fill = NULL),
    expected = c(NA, NA, NA, NA, 3, 4)
  )
  expect_equal(
    x |> rolling("min", .width = 3, .min_non_na = 2, .fill = NULL),
    expected = c(NA, NA, NA, NA, 2, 2)
  )
})

test_that("vectorized quantile/median respect .min_non_na", {
  x <- c(1, NA, NA, 2, 3, 4)
  expect_equal(
    x |> rolling("quantile", .width = 3, .min_non_na = 2, probs = 0.5, .fill = NULL),
    expected = c(NA, NA, NA, NA, 2.5, 3)
  )
  expect_equal(
    x |> rolling("median", .width = 3, .min_non_na = 2, .fill = NULL),
    expected = c(NA, NA, NA, NA, 2.5, 3)
  )
  # Full windows only
  expect_equal(
    x |> rolling("median", .width = 3, .min_non_na = 3, .fill = NULL),
    expected = c(NA, NA, NA, NA, NA, 3)
  )
})

test_that("width 1 has no partial windows (fill not applied)", {
  expect_equal(
    (1:5) |> rolling("sum", .width = 1, .fill = -1),
    expected = 1:5
  )
  expect_equal(
    (1:5) |> rolling("max", .width = 1, .fill = -1),
    expected = 1:5
  )
  expect_equal(
    (1:5) |> rolling("sum", .width = 1, .direction = "forward", .fill = -1),
    expected = 1:5
  )
})

test_that("vectorized FUNs place .fill at the correct end for forward windows", {
  # Matches zoo::rollapply(1:5, width = 3, align = "left", fill = -1)
  expect_equal(
    (1:5) |> rolling("sum", .width = 3, .direction = "forward", .fill = -1),
    expected = c(6, 9, 12, -1, -1)
  )
  expect_equal(
    (1:5) |> rolling("max", .width = 3, .direction = "forward", .fill = -1),
    expected = c(3, 4, 5, -1, -1)
  )
  expect_equal(
    (1:5) |> rolling("min", .width = 3, .direction = "forward", .fill = -1),
    expected = c(1, 2, 3, -1, -1)
  )
  expect_equal(
    (1:5) |> rolling("mean", .width = 3, .direction = "forward", .fill = -1),
    expected = c(2, 3, 4, -1, -1)
  )
})

test_that("vectorized mean returns .fill verbatim (not divided by window count)", {
  x <- c(1, 2, NA, 4, 5)
  # Matches zoo::rollapply with align = "left"/"right" and FUN = mean(na.rm = TRUE)
  expect_equal(
    x |> rolling("mean", .width = 3, .direction = "forward", .fill = -1),
    expected = zoo::rollapply(x, 3, \(z) mean(z, na.rm = TRUE), fill = -1, align = "left")
  )
  expect_equal(
    x |> rolling("mean", .width = 3, .fill = -1),
    expected = zoo::rollapply(x, 3, \(z) mean(z, na.rm = TRUE), fill = -1, align = "right")
  )
})

test_that("width larger than x (all windows partial) returns .fill, matching zoo", {
  x <- 1:3
  for (direction in c("backward", "forward")) {
    expect_equal(
      x |> rolling("sum", .width = 5, .direction = direction, .fill = -1),
      expected = c(-1, -1, -1)
    )
    expect_equal(
      x |> rolling("max", .width = 5, .direction = direction, .fill = -1),
      expected = c(-1, -1, -1)
    )
    expect_equal(
      x |> rolling("mean", .width = 5, .direction = direction, .fill = -1),
      expected = c(-1, -1, -1)
    )
    expect_equal(
      x |> rolling("median", .width = 5, .direction = direction, .fill = -1),
      expected = c(-1, -1, -1)
    )
    # Default fill is NA
    expect_true(all(is.na(x |> rolling("sum", .width = 5, .direction = direction))))
  }
})

test_that("empty x returns an empty vector", {
  expect_equal(rolling(numeric(0), "sum", .width = 3), numeric(0))
  expect_equal(rolling(numeric(0), "sum", .width = 3, .fill = -1), numeric(0))
  expect_equal(rolling(numeric(0), "max", .width = 3), numeric(0))
  expect_equal(rolling(numeric(0), "mean", .width = 3), numeric(0))
  expect_equal(rolling(numeric(0), "median", .width = 3), numeric(0))
  expect_equal(rolling(numeric(0), "quantile", .width = 3, probs = 0.5), numeric(0))
})
