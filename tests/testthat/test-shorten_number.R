test_that("basic case works", {
  x <- c(
    1,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000
  )
  expected <- c(
    "1",
    "100",
    "1K",
    "10K",
    "100K",
    "1M",
    "10M",
    "100M",
    "1B",
    "10B"
  )
  expect_equal(
    shorten_number(x),
    expected = expected
  )
})

test_that("reverse works", {
  x <- c("1", "100", "1K", "10K", "100K", "1M", "10M", "100M", "1B", "10B")
  expected <- c(
    1,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000
  )
  expect_equal(
    shorten_number(x, reverse = TRUE),
    expected = expected
  )
})

test_that("reverse works without coercion warnings", {
  x <- c("1", "100", "1K", "10K", "100K", "1M", "10M", "100M", "1B", "10B")
  expect_no_warning(
    result <- shorten_number(x, reverse = TRUE)
  )
  expect_equal(
    result,
    expected = c(1, 100, 1000, 10000, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10)
  )
})

test_that("reverse still warns for genuinely invalid input", {
  expect_warning(
    shorten_number(c("1K", "not a number"), reverse = TRUE),
    regexp = "NAs introduced by coercion"
  )
})

test_that("custom thresholds work", {
  x <- c(
    1,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000
  )

  thresholds <- list("cats" = 1e3, "dogs" = 1e6, "fish" = 1e9)
  expected <- c(
    "1",
    "100",
    "1cats",
    "10cats",
    "100cats",
    "1dogs",
    "10dogs",
    "100dogs",
    "1fish",
    "10fish"
  )

  expect_equal(
    shorten_number(x, thresholds = thresholds),
    expected = expected
  )
})
