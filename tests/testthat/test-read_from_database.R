test_that("basic case works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)
  db <- db_list[[1]]

  # Read data from database
  output <- read_from_database(db, table_name = "airquality") |> 
    as.data.frame()
  expect_equal(output, datasets::airquality, tolerance = 0.0001)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(db_path)
})

test_that("custom query works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)
  db <- db_list[[1]]

  # Read data from database
  db_query <- \(df) df |> dplyr::filter(.data$Month == 5)
  output <- db |> 
    read_from_database(table_name = "airquality", query_fun = db_query) |>
    as.data.frame()
  expect_equal(output, datasets::airquality |>
    dplyr::filter(Month == 5), tolerance = 0.0001)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(db_path)
})

test_that("collect works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)
  db <- db_list[[1]]

  # Read data from database as lazy tbl
  output <- db |>
    read_from_database(table_name = "airquality", collect = FALSE)
  expect_true("tbl_lazy" %in% class(output))

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(db_path)
})

test_that("pull works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)
  db <- db_list[[1]]

  # Pull Month column using a character
  output <- db |>
    read_from_database(table_name = "airquality", pull = "Month")
  expect_equal(output, datasets::airquality$Month)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(db_path)
})
