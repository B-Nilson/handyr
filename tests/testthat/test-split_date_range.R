test_that("basic case works", {
  date_range <- as.Date(c("2020-01-01", "2020-01-12"))
  expect_snapshot(
    split_date_range(date_range, max_duration = "3 days")
  )
  expect_snapshot(
    split_date_range(date_range, max_duration = "3 days", as_list = TRUE)
  )
})
