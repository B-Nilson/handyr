test_that("basic case works", {
  expect_message(log_step("Hello, world!"))
  expect_type(log_step("Hello, world!"), "list")
  expect_length(log_step("Hello, world!"), 3)
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