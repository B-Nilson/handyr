test_that("basic case works", {
  expect_equal(
    convert_units(c(1, 2, 3), from = "m", to = "km"),
    c(0.001, 0.002, 0.003)
  )
})

test_that("keep_units works", {
  expect_equal(
    convert_units(c(1, 2, 3), from = "m", to = "km", keep_units = TRUE),
    c(0.001, 0.002, 0.003) |> units::set_units("km")
  )
  expect_equal(
    1:5 |> units::set_units("cm") |> convert_units(to = "m"),
    (1:5 / 100) |> units::set_units("m")
  )
})
