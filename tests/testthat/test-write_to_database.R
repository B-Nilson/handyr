test_that("writing to SQLite works", {
  db_name <- "__test.sqlite"
  db_dir <- tempdir()
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
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  # Check data in database matches input
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame() |>
    expect_equal(airquality, tolerance = 0.0001)
  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(file.path(db_dir, db_name))
})

test_that("writing to duckdb works", {
  db_name <- "__test.duckdb"
  db_dir <- tempdir()
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
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  # Check data in database matches input
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame() |>
    expect_equal(airquality, tolerance = 0.0001)
  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(file.path(db_dir, db_name))
})

test_that("writing to postgresql works", {
  skip("Skipping PostgreSQL tests as they download/invoke external binaries")
  db_name <- "__test.postgresql"
  db_dir <- tempdir()
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
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  # Check data in database matches input
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    as.data.frame() |>
    expect_equal(airquality, tolerance = 0.0001)
  # Cleanup
  DBI::dbDisconnect(db)
})
