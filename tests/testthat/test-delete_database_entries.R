test_that("basic case works", {
  # Create temp db to work with
  db_list <- init_airquality_sqlite_test()
  db_path <- names(db_list)
  db <- db_list[[1]]

  # Delete May 5th entry
  entry_keys <- data.frame(Month = 5, Day = 5)
  db |>
    delete_database_entries(table_name = "airquality", entry_keys = entry_keys)

  # Check it is gone but rest is there
  read_from_database(db, table_name = "airquality") |>
    as.data.frame() |>
    expect_equal(
      datasets::airquality |>
        dplyr::filter(!(Month == 5 & Day == 5)),
      tolerance = 0.0001
    )
  
  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(db_path)
})
