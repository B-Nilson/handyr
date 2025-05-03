test_that("basic case works", {
  expect_equal(
    do_if(c(1, 2, 3), mean, .if = TRUE),
    expected = 2
  )
  expect_equal(
    do_if(c(1, 2, 3), mean, .if = FALSE),
    expected = NA
  )
})

test_that(".return works", {
  expect_equal(
    do_if(c(1, 2, 3), mean, .if = FALSE, .return = -1),
    expected = -1
  )
})

test_that("... works", {
  expect_equal(
    do_if(c(1, 2, 3), .do = clamp, range = c(2, 4), .if = TRUE),
    expected = c(2, 2:3)
  )
})
