test_that("basic case works", {
  values <- 1:3
  expect_equal(
    values |> for_each(\(value) value + 1),
    expected = 2:4 |> as.list()
  )
})

test_that(".bind works", {
  dats <- list(
    data.frame(x = 1:3),
    data.frame(x = 4:6)
  )
  expect_equal(
    dats |>
      for_each(
        \(dat) dat |> dplyr::mutate(y = x + 1),
        .bind = TRUE
      ),
    expected = data.frame(
      x = 1:6,
      y = 2:7
    )
  )
})

test_that(".name works", {
  expect_equal(
    c("bread", "jam") |>
      for_each(
        \(value) paste("eat", value),
        .name = TRUE
      ),
    expected = list(bread = "eat bread", jam = "eat jam")
  )
})

test_that(".parallel/.workers works", {
  values <- 1:3
  expect_equal(
    values |> for_each(\(value) value + 1, .parallel = TRUE),
    expected = 2:4 |> as.list()
  )
  expect_equal(
    values |> for_each(\(value) value + 1, .parallel = TRUE, .workers = 2),
    expected = 2:4 |> as.list()
  )
})

test_that(".quiet works", {
  values <- 1:3
  expect_no_message(expect_invisible(expect_equal(
    values |> for_each(\(value) {message(value + 1); value + 1}, .quiet = TRUE),
    expected = 2:4 |> as.list()
  )))
})
