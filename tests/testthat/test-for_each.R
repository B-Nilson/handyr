test_that("basic case works", {
  values <- 1:3
  expect_equal(
    values |> for_each(\(value) value + 1),
    expected = 2:4
  )
})

test_that(".enumerate works", {
  values <- c(5, 5, 5)
  expect_equal(
    values |> for_each(\(value, i) value + i, .enumerate = TRUE),
    expected = 6:8
  )
  expect_equal(
    values |> for_each(\(value, i) value + i, .enumerate = TRUE, .quiet = TRUE),
    expected = 6:8
  )
})

test_that(".bind and .bind_id work", {
  dats <- list(
    one = data.frame(x = 1:3),
    two = data.frame(x = 4:6)
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
  expect_equal(
    dats |>
      for_each(
        \(dat) dat |> dplyr::mutate(y = x + 1),
        .bind = TRUE,
        .bind_id = "id"
      ),
    expected = data.frame(
      id = c("one", "one", "one", "two", "two", "two"),
      x = 1:6,
      y = 2:7
    )
  )
  # make sure .bind = TRUE works when non-list provided as input
  expect_equal(
    1:3 |> for_each(\(value) data.frame(value = value + 1), .bind = TRUE),
    expected = data.frame(value = 2:4)
  )
})

test_that(".name works", {
  expect_equal(
    c("bread", "jam") |>
      for_each(
        \(value) paste("eat", value),
        .name = TRUE
      ),
    expected = c(bread = "eat bread", jam = "eat jam")
  )
})

test_that(".as_list works", {
  expect_equal(
    c("bread", "jam") |>
      for_each(
        \(value) paste("eat", value),
        .as_list = TRUE
      ),
    expected = list("eat bread", "eat jam")
  )
})

test_that(".parallel/.workers/.plan/.clean works", {
  values <- 1:3
  expect_equal(
    values |> for_each(\(value) value + 1, .parallel = TRUE),
    expected = 2:4
  )
  expect_equal(
    values |> for_each(\(value) value + 1, .parallel = TRUE, .workers = 2),
    expected = 2:4
  )
  expect_true(is(future::plan(), "sequential"))
  expect_equal(
    values |> for_each(\(value) value + 1, .parallel = TRUE, .workers = 2, .plan = "multicore", .parallel_cleanup = FALSE),
    expected = 2:4
  )
  expect_true(is(future::plan(), "multicore"))
  future::plan("sequential")
})

test_that(".quiet works", {
  values <- 1:3
  expect_no_message(expect_invisible(expect_equal(
    values |> for_each(\(value) {
      message(value + 1)
      value + 1
    }, .quiet = TRUE),
    expected = 2:4
  )))
})
