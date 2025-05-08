test_that("basic case works", {
  expect_equal(expect_no_error(expect_invisible(expect_no_message(
    capture_output(silence({
      warning("test warning")
      message("test message")
      print("test print")
      cat("test cat\n")
      writeLines("test writeLines")
    }))
  ))), expected = "")
})

test_that("all set to FALSE just returns invisible(...)", {
  expect_equal(expect_invisible(
    silence(1 + 2, errors = FALSE, warnings = FALSE, messages = FALSE, output = FALSE)
  ), 3)
})
