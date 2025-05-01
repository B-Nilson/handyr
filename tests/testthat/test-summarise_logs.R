test_that("basic case works", {
  logs <- list(
    log_step("Header", header = TRUE, quiet = TRUE),
    log_step("test1", quiet = TRUE),
    log_step("test2", quiet = TRUE),
    log_step("test3", quiet = TRUE),
    log_step("test4", quiet = TRUE),
    log_step("test5", quiet = TRUE),
    log_step('Complete', quiet = TRUE)
  )
  expect_message(
    summarise_logs(logs)
  )
})
