test_that("basic case works", {
  logs <- list(
    log_step("Header", header = TRUE, quiet = TRUE)[[1]],
    log_step("test1", quiet = TRUE),
    log_step("\tsome result", time = FALSE, quiet = TRUE),
    log_step("test2", quiet = TRUE),
    log_step("\tsome result", time = FALSE, quiet = TRUE),
    log_step("\tsome result", time = FALSE, quiet = TRUE),
    log_step("\tsome result", time = FALSE, quiet = TRUE),
    log_step("test3", quiet = TRUE),
    log_step("test4", quiet = TRUE),
    log_step("test5", quiet = TRUE),
    log_step("Complete", quiet = TRUE)
  )
  expect_message(
    summarise_logs(logs)
  ) |>
    expect_snapshot()
})

test_that("wrapped header entry (README usage) works", {
  logs <- list(
    log_step("My Script", header = TRUE, time = FALSE, quiet = TRUE), # wrapped in .log_init
    log_step("step 1", quiet = TRUE),
    log_step("step 2", quiet = TRUE)
  )
  log_summary <- suppressMessages(summarise_logs(logs))
  # Header entry is unwrapped so its text/timestamp are kept
  expect_equal(nrow(log_summary), 3)
  expect_equal(log_summary$text[1], "My Script")
  expect_true(log_summary$is_header[1])
  expect_false(is.na(log_summary$timestamp[1]))
  # No NA header row polluting the timed sections
  sections <- log_summary[
    !log_summary$is_not_section & log_summary$.id != max(log_summary$.id),
  ]
  expect_equal(nrow(sections), 1)
  expect_equal(sections$text, "step 1")
})

test_that("total time includes the header-to-first-step period", {
  logs <- list(
    log_step("My Script", header = TRUE, time = FALSE, quiet = TRUE)
  )
  # Sleep past a second boundary so the (second-precision) timestamps can
  # distinguish "header to last entry" from "first step to last entry"
  Sys.sleep(1.1)
  logs$step1 <- log_step("step 1", quiet = TRUE)
  logs$done <- log_step("done", quiet = TRUE)

  log_summary <- suppressMessages(summarise_logs(logs))
  msgs <- testthat::capture_messages(summarise_logs(logs))

  # The total should span the header (start) to the last entry
  expected_total <- prettyunits::pretty_sec(as.numeric(
    difftime(
      log_summary$timestamp[nrow(log_summary)],
      log_summary$timestamp[1],
      units = "secs"
    )
  ))
  expect_true(any(grepl(
    paste0("Total time: ", expected_total),
    msgs,
    fixed = TRUE
  )))
  # The header-to-first-step gap is counted: total is never just the
  # (≈0s) gap between the two steps
  expect_false(any(grepl("Total time: 0", msgs, fixed = TRUE)))
})

test_that("sub-second run times are captured", {
  logs <- list(
    log_step("Header", header = TRUE, time = FALSE, quiet = TRUE)
  )
  logs$step1 <- log_step("step 1", quiet = TRUE)
  Sys.sleep(0.4)
  logs$step2 <- log_step("step 2", quiet = TRUE)

  log_summary <- suppressMessages(summarise_logs(logs))
  step1_run <- log_summary$run_time[log_summary$text == "step 1"]
  # step1's run time spans the ~0.4s sleep (was 0 before sub-second precision)
  expect_true(!is.na(step1_run))
  expect_gt(as.numeric(step1_run, units = "secs"), 0.3)
})

test_that("save_to works", {
  logs <- list(
    log_step("Header", header = TRUE, quiet = TRUE)[[1]],
    log_step("test1", quiet = TRUE),
    log_step("test2", quiet = TRUE),
    log_step("test3", quiet = TRUE),
    log_step("test4", quiet = TRUE),
    log_step("test5", quiet = TRUE),
    log_step("Complete", quiet = TRUE)
  )
  temp_file <- tempfile()
  log_summary <- summarise_logs(logs, save_to = temp_file)
  expect_true(file.exists(temp_file))
})
