test_that("basic case works", {
  urls <- c("blargh", "https://www.google.com/")
  check_urls_exist(urls, quiet = TRUE) |>
    expect_no_warning() |>
    expect_equal(c(FALSE, TRUE) |> stats::setNames(urls))
})

test_that("quiet arg works", {
  urls <- c("blargh", "https://www.google.com/")
  check_urls_exist(urls[1], quiet = FALSE) |>
    expect_warning()
  check_urls_exist(urls[2], quiet = FALSE) |>
    expect_no_warning() |>
    expect_equal(TRUE |> stats::setNames(urls[2]))
})

test_that("invalid args fail", {
  check_urls_exist(c("blargh", NA), quiet = TRUE) |>
    expect_error("`urls` must not contain missing values.")
  check_urls_exist(list("blargh"), quiet = TRUE) |>
    expect_error("`urls` must be a character vector.")
  check_urls_exist(character(0), quiet = TRUE) |>
    expect_error("At least one URL must be provided.")
  check_urls_exist(NULL, quiet = TRUE) |>
    expect_error("At least one URL must be provided.")
  check_urls_exist("https://www.google.com/", quiet = NULL) |>
    expect_error("`quiet` must be a single logical value.")
  check_urls_exist("https://www.google.com/", quiet = c(TRUE, FALSE)) |>
    expect_error("`quiet` must be a single logical value.")
})
