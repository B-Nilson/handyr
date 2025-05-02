test_that("basic case works", {
  expect_no_warning(expect_no_error(
    on_warning(warning("test"))
  ))
})

test_that(".return works", {
  expect_equal(expect_no_error(
    on_warning(warning("test"), .return = -1)
  ), expected = -1)
})

test_that(".message works", {
  expect_message(expect_no_error(
    on_warning(warning("test"), .message = TRUE)
  ))
})

test_that(".warn works", {
  expect_error(expect_no_warning(
    on_warning(warning("test"), .stop = TRUE)
  ))
})
