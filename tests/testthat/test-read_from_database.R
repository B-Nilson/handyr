test_that("basic case works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "duckdb")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  expected <- db_list[[2]]
  on.exit(\(...){
    DBI::dbDisconnect(db)
    file.remove(db_path)
  })

  # Read data from database
  read_from_database(db, table_name = "airquality") |>
    as.data.frame() |>
    dplyr::arrange(date) |>
    expect_equal(expected, tolerance = 0.0001)
})

test_that("custom query works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "duckdb")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  expected <- db_list[[2]]
  on.exit(\(...){
    DBI::dbDisconnect(db)
    file.remove(db_path)
  })

  # Read data from database
  db_query <- \(df) df |> dplyr::filter(.data$Month == 5)
  db |>
    read_from_database(table_name = "airquality", query_fun = db_query) |>
    as.data.frame() |>
    dplyr::arrange(date) |>
    expect_equal(expected |> dplyr::filter(Month == 5), tolerance = 0.0001)
})

test_that("collect works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)
  db <- db_list[[1]]
  on.exit(\(...){
    DBI::dbDisconnect(db)
    file.remove(db_path)
  })

  # Read data from database as lazy tbl
  output <- db |>
    read_from_database(table_name = "airquality", collect = FALSE)
  expect_true("tbl_lazy" %in% class(output))
})

test_that("pull works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)
  db <- db_list[[1]]
  on.exit(\(...){
    DBI::dbDisconnect(db)
    file.remove(db_path)
  })
  # Pull Month column using a character
  output <- db |>
    read_from_database(table_name = "airquality", pull = "Month")
  expect_equal(output, datasets::airquality$Month)
})
