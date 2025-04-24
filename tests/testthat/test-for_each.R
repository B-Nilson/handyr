test_that("basic case works", {
  values = 1:3
  expect_no_warning(expect_no_error(
    values |> for_each(\(value) value + 1)
  ))
})

test_that(".bind works", {
  dats = list(
    data.frame(x = 1:3),
    data.frame(x = 4:6)
  )
  expect_no_warning(expect_no_error(
    dats |> 
      for_each(
        \(dat) dat |> dplyr::mutate(y = x + 1), 
        .bind = TRUE
      )
  ))
})

test_that(".name works", {
  values = c("bread", "jam")
  expect_no_warning(expect_no_error(
    values |> 
      for_each(
        \(value) paste("hello", value), 
        .name = TRUE
      )
  ))
})

test_that(".parallel/.workers works", {
  values = 1:3
  expect_no_warning(expect_no_error(
    values |> for_each(\(value) value + 1, .parallel = TRUE, .workers = 2)
  ))
})

test_that(".invisible works", {
  values = 1:3
  expect_no_warning(expect_no_message(expect_no_error(expect_invisible(
    values |> for_each(\(value) message(value + 1), .invisible = TRUE)
  ))))
})
