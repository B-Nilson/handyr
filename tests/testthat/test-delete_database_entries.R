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

  # Delete May 5th entry
  entry_keys <- data.frame(Month = c(5, 5), Day = c(4, 5))
  db |>
    delete_database_entries(table_name = "airquality", entry_keys = entry_keys)

  # Check it is gone but rest is there
  read_from_database(db, table_name = "airquality") |>
    as.data.frame() |>
    expect_equal(
      expected |>
        dplyr::filter(!(Month == 5 & Day %in% c(4, 5))),
      tolerance = 0.0001
    )
})
