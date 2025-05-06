test_that("basic case works", {
  expect_message(log_step("Hello, world!"))
  expect_type(log_step("Hello, world!"), "list")
  expect_length(log_step("Hello, world!"), 3)
})

test_that("... works", {
  expect_message(log_step("Hello", "world!"), "Hello world!")
  expect_message(
    log_step(
      list(1, 2, 3),
      c("test", "test"),
      1:3,
      NA, NULL, Inf,
      data.frame(x = 1:3, y = as.character(1:3)),
      as.Date("2000-01-01"),
      sep = "-", time = FALSE
    ),
    "1-2-3-test-test-1-2-3-Inf-1-2-3-1-2-3-10957"
  )
})

test_that("time works", {
  expect_message(log_step("Hello, world!", time = TRUE))
  result <- log_step("Hello, world!", time = TRUE)
  expect_equal(
    paste0(result$timestamp, ": ", result$text),
    result$message
  )
})

test_that("time_format works", {
  expect_message(log_step("Hello, world!", time_format = "%Y-%m-%d"))
  result <- log_step("Hello, world!", time_format = "%Y-%m-%d", quiet = TRUE)
  expect_equal(
    paste0(result$timestamp, ": ", result$text),
    result$message
  )
  expect_message(log_step("Hello, world!", time_format = "%Y-%m-%d %H:%M"))
  result <- log_step("Hello, world!", time_format = "%Y-%m-%d %H:%M", quiet = TRUE)
  expect_equal(
    paste0(
      format(result$timestamp, "%Y-%m-%d %H:%M"), ": ",
      result$text
    ),
    result$message
  )
})

test_that("tz works", {
  expect_message(log_step("Hello, world!", tz = "America/New_York"))
  result <- log_step("Hello, world!", tz = "America/New_York", quiet = TRUE)
  expect_equal(
    paste0(result$timestamp, ": ", result$text),
    result$message
  )
  expect_equal(
    attr(result$timestamp, "tzone"),
    "America/New_York"
  )
})

test_that("header works", {
  expect_message(log_step("Hello, world!", header = TRUE))
  result <- log_step("Hello, world!", header = TRUE, time = FALSE, quiet = TRUE)
  expect_equal(
    paste0("   ", result$text, "   "),
    gsub("[\\||-]", "", result$message)
  )
})

test_that("quiet works", {
  expect_no_message(log_step("Hello, world!", quiet = TRUE))
})

test_that("sep works", {
  expect_message(log_step(c("Hello", "world!"), sep = ", "), regexp = "Hello, world!")
})

test_that("header triggers wrapping in additional list", {
  result <- log_step("Hello, world!", header = TRUE, quiet = TRUE)
  expect_equal(names(result), ".log_init")
  expect_type(result$.log_init, "list")
  expect_type(result, "list")
})
