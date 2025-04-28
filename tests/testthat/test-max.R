test_that("basic case works", {
  expect_length(
    max(1:5),
    n = 1
  )
  expect_equal(
    max(1:5),
    expected = 5
  )
  expect_length(
    max(NA_real_, NA_real_, NA_real_),
    n = 1
  )
  expect_equal(
    max(NA_real_, NA_real_, NA_real_),
    expected = NA_real_
  )
})

test_that("na.rm works", {
  expect_equal(
    max(NA_real_, NA_real_, NA_real_, na.rm = TRUE),
    expected = NA_real_
  )
  expect_equal(
    max(NA_real_, NA_real_, 1, na.rm = FALSE),
    expected = NA_real_
  )
  expect_equal(
    max(NA_real_, NA_real_, NA_real_, 1, na.rm = TRUE),
    expected = 1
  )
})

test_that("correct NA returned", {
  expect_equal(
    max(NA_real_, NA_real_, 1.1),
    expected = NA_real_
  )
  expect_equal(
    max(NA_integer_, NA_integer_, 1),
    expected = NA_integer_
  )
  expect_equal(
    max(NA_character_, NA_character_, "test"),
    expected = NA_character_
  )
})
