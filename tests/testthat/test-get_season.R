test_that("basic case works", {
  dates <- as.Date(c(
    "2021-01-01",
    "2021-04-01",
    "2021-07-01",
    "2021-10-01",
    "2021-12-01"
  ))
  expect_equal(
    get_season(dates),
    c("Winter", "Spring", "Summer", "Fall", "Winter")
  )
})

test_that("include_year works", {
  dates <- as.Date(c(
    "2021-01-01",
    "2021-04-01",
    "2021-07-01",
    "2021-10-01",
    "2021-12-01"
  ))
  expect_equal(
    get_season(dates, include_year = TRUE),
    c("Winter 2020", "Spring 2021", "Summer 2021", "Fall 2021", "Winter 2021")
  )
})

test_that("include_months works", {
  dates <- as.Date(c(
    "2021-01-01",
    "2021-04-01",
    "2021-07-01",
    "2021-10-01",
    "2021-12-01"
  ))
  expect_equal(
    get_season(dates, include_months = TRUE),
    c(
      "Winter [DJF]",
      "Spring [MAM]",
      "Summer [JJA]",
      "Fall [SON]",
      "Winter [DJF]"
    )
  )
})

test_that("use_autumn works", {
  dates <- as.Date(c(
    "2021-01-01",
    "2021-04-01",
    "2021-07-01",
    "2021-10-01",
    "2021-12-01"
  ))
  expect_equal(
    get_season(dates, use_autumn = TRUE),
    c("Winter", "Spring", "Summer", "Autumn", "Winter")
  )
})

test_that("as_factor works", {
  dates <- as.Date(c(
    "2021-01-01",
    "2021-04-01",
    "2021-07-01",
    "2021-10-01",
    "2021-12-01"
  ))
  expect_equal(
    get_season(dates, as_factor = TRUE),
    c("Winter", "Spring", "Summer", "Fall", "Winter") |>
      factor(levels = c("Spring", "Summer", "Fall", "Winter"))
  )
  expect_equal(
    get_season(dates, as_factor = TRUE, include_year = TRUE),
    c(
      "Winter 2020",
      "Spring 2021",
      "Summer 2021",
      "Fall 2021",
      "Winter 2021"
    ) |>
      factor(
        levels = c(
          "Winter 2020",
          "Spring 2021",
          "Summer 2021",
          "Fall 2021",
          "Winter 2021"
        )
      )
  )
  expect_equal(
    get_season(dates, as_factor = TRUE, include_months = TRUE),
    c(
      "Winter [DJF]",
      "Spring [MAM]",
      "Summer [JJA]",
      "Fall [SON]",
      "Winter [DJF]"
    ) |>
      factor(
        levels = c("Spring [MAM]", "Summer [JJA]", "Fall [SON]", "Winter [DJF]")
      )
  )
  levels <- c(
    "Winter 2020 [DJF]",
    "Spring 2021 [MAM]",
    "Summer 2021 [JJA]",
    "Fall 2021 [SON]",
    "Winter 2021 [DJF]"
  )
  expect_equal(
    get_season(
      dates,
      as_factor = TRUE,
      include_year = TRUE,
      include_months = TRUE
    ),
    levels |> factor(levels = levels)
  )
})
