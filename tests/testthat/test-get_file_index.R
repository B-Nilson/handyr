test_that("basic case works", {
  url <- "https://aqmap.ca/aqmap/outputs/"
  get_file_index(url) |>
    expect_no_error() |>
    expect_length(n = 4)

  url <- "https://google.com"
  get_file_index(url) |>
    expect_error()
})
