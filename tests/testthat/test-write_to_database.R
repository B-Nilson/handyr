test_that("writing to SQLite works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  expected <- db_list[[2]]

  # Check data in database matches input
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame() |>
    dplyr::mutate(
      date = as.Date(date),
      datetime = as.POSIXct(datetime, tz = "UTC")
    ) |>
    dplyr::arrange(date) |> 
    expect_equal(expected, tolerance = 0.0001)
  
  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(file.path(db_path))
})

test_that("writing to duckdb works", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "duckdb")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  expected <- db_list[[2]]

  # Check data in database matches input
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame() |>
    expect_equal(expected, tolerance = 0.0001)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(file.path(db_path))
})

test_that("writing to postgresql works", {
  skip("Skipping PostgreSQL tests as they download/invoke external binaries")
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "postgresql")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  expected <- db_list[[2]]

  # Check data in database matches input
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame() |>
    expect_equal(expected, tolerance = 0.0001)
  
  # Cleanup
  DBI::dbDisconnect(db)
})

test_that("insert_new only inserts non-overlapping entries", {
  db_name <- "__test.sqlite"
  db_dir <- tempdir()
  new_data_1 <- airquality[5:12, ]
  new_data_2 <- airquality[1:8, ]
  new_data_2$Ozone <- new_data_2$Ozone * 2
  row.names(new_data_1) <- NULL
  row.names(new_data_2) <- NULL

  # Create database
  db <- create_database(
    name = db_name,
    return_connection = TRUE,
    path = db_dir
  )
  # Write data to database
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = new_data_1,
      primary_keys = c("Month", "Day")
    )
  # Write overlapping (modified Ozone) data to database, overlap should NOT be modified
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = new_data_2,
      primary_keys = c("Month", "Day"),
      insert_new = TRUE,
      update_duplicates = FALSE
    )
  # Check data in database matches expected
  db_data <- dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame()
  expected_data <- rbind(new_data_1, new_data_2) |>
    dplyr::distinct(Month, Day, .keep_all = TRUE)
  db_data |> expect_equal(expected_data, tolerance = 0.0001)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(file.path(db_dir, db_name))
})

test_that("update_duplicates only inserts overlapping entries", {
  db_name <- "__test.sqlite"
  db_dir <- tempdir()
  new_data_1 <- airquality[1:8, ]
  new_data_2 <- airquality[5:12, ]
  new_data_2$Ozone <- new_data_2$Ozone * 2
  row.names(new_data_1) <- NULL
  row.names(new_data_2) <- NULL

  # Create database
  db <- create_database(
    name = db_name,
    return_connection = TRUE,
    path = db_dir
  )
  # Write data to database
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = new_data_1,
      primary_keys = c("Month", "Day")
    )
  # Write ONLY overlapping (modified Ozone) data to database
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = new_data_2,
      primary_keys = c("Month", "Day"),
      insert_new = FALSE,
      update_duplicates = TRUE
    )

  # Check data in database matches expected
  db_data <- dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame()
  expected_data <- new_data_1[1:4, ] |>
    rbind(new_data_2[1:4, ])
  db_data |> expect_equal(expected_data, tolerance = 0.0001)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(file.path(db_dir, db_name))
})
