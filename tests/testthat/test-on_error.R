test_that("basic case works", {
  expect_no_warning(expect_no_error(
    on_error(stop())
  ))
})

test_that(".return works", {
  expect_equal(
    expect_no_error(
      on_error(stop(), .return = -1)
    ),
    expected = -1
  )
})

test_that(".message works", {
  expect_message(expect_no_error(
    on_error(stop(), .message = TRUE)
  ))
})

test_that(".warn works", {
  expect_warning(expect_no_error(
    on_error(stop(), .warn = TRUE)
  ))
})

test_that("custom message works", {
  expect_message(expect_no_error(
    on_error(stop(), .message = "test")
  ), "test")

  expect_warning(expect_no_error(
    on_error(stop(), .warn = "test")
  ), "test")

  expect_error(
    on_warning(warning(), .stop = "test"), 
    "test"
  )

  expect_message(
    on_warning(warning(), .message = "test"), 
    "test"
  )
})
