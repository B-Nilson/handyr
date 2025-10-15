test_that("basic case works", {
  community <- expect_no_warning(expect_no_error(
    get_communities("Fort St. John, BC", quiet = TRUE)
  ))

  expect_true(nrow(community) == 1)
  expect_equal(community$name, "Fort St. John")
  expect_equal(
    community$geometry |> unlist() |> unname(),
    c(-120.8469430, 56.2524039)
  )
})

test_that("able to get all canadian communities", {
  skip("Takes a long time to run")

  prov_terrs <- c(
    "British Columbia",
    "Alberta",
    "Saskatchewan",
    "Manitoba",
    "Ontario",
    "Quebec",
    "New Brunswick",
    "Nova Scotia",
    "Prince Edward Island",
    "Newfoundland and Labrador",
    "Yukon",
    "Northwest Territories",
    "Nunavut"
  ) |>
    paste0(", Canada")

  communities <- expect_no_warning(expect_no_error(
    prov_terrs |>
      for_each(get_communities, .name = TRUE, .bind_id = "prov_terr")
  ))

  expect_true(nrow(communities) > 1000)
  expect_true(all(!is.na(communities$name)))
})
