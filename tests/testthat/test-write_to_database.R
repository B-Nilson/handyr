test_that("writing to SQLite works", {
  db_path <- "__test.sqlite"
  db <- create_database(
    name = db_path,
    return_connection = TRUE,
    path = tempdir()
  )
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    expect_equal(airquality)
  DBI::dbDisconnect(db)
  file.remove(db_path)
})

test_that("writing to duckdb works", {
  db_path <- "__test.duckdb"
  db <- create_database(
    name = db_path,
    return_connection = TRUE,
    path = tempdir()
  )
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    expect_equal(airquality)
  DBI::dbDisconnect(db)
  file.remove(db_path)
})

test_that("writing to postgresql works", {
  skip("Skipping PostgreSQL tests as they download/invoke external binaries")
  # TODO: ned function to remove postgress databases
  db_name <- "__test.postgresql"
  db <- create_database(name = db_name)
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  dplyr::tbl(db, "airquality") |>
    dplyr::collect() |>
    expect_equal(airquality)
  DBI::dbDisconnect(db)
})
