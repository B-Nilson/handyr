test_that("basic case works", {
  logs <- list(
    log_step("test", quiet = TRUE),
    log_step("test", quiet = TRUE),
    log_step("test", quiet = TRUE),
    log_step("test", quiet = TRUE),
    log_step("test", quiet = TRUE),
    log_step('test', quiet = TRUE)
  )
  expect_message(
    summarise_logs(logs)
  )
})
