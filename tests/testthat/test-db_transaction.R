test_that("basic transaction works", {
  # Create temp db to work with
  db_list <- init_airquality_sqlite_test()
  db_path <- names(db_list)
  db <- db_list[[1]]

  # No error introduced by function
  db |> 
    db_transaction({
      db |> read_from_database(table_name = "airquality")
    }) |> 
    as.data.frame() |>
    expect_equal(airquality, tolerance = 0.0001)

  # TODO: Test that error in transaction does not bork db

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(db_path)
})
