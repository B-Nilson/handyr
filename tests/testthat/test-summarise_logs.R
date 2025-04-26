test_that("basic case works", {
  logs <- list(
    log("test", quiet = TRUE),
    log("test", quiet = TRUE),
    log("test", quiet = TRUE),
    log("test", quiet = TRUE),
    log("test", quiet = TRUE),
    log('test', quiet = TRUE)
  )
  expect_message(
    summarise_logs(logs)
  )
})
