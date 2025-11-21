test_that("indexes work", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "duckdb")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  withr::defer({
    DBI::dbDisconnect(db)
    file.remove(db_path)
  })

  # Make dataset to insert
  new_data <- mtcars |>
    as.data.frame() |>
    dplyr::mutate(car = row.names(mtcars))
  row.names(new_data) <- NULL

  # Create table
  db |>
    create_database_table(
      table_name = "mtcars",
      new_data = new_data,
      primary_keys = c("car"),
      indexes = list(
        c("cyl", "disp"),
        efficiency = c("mpg", "hp")
      )
    )

  # Check indexes are created
  db |> # TODO: abstract this and export
    DBI::dbGetQuery(
      "SELECT * FROM duckdb_indexes() WHERE table_name = 'mtcars'"
    ) |>
    dplyr::select(table_name, index_name, expressions, is_primary, is_unique) |>
    dplyr::arrange(table_name, index_name) |>
    expect_equal(
      data.frame(
        table_name = "mtcars",
        index_name = c("mtcars_cyl_disp", "mtcars_efficiency"),
        expressions = list(c("cyl", "disp"), c("mpg", "hp")) |>
          sapply(\(x) paste0("[", paste(x, collapse = ", "), "]")),
        is_primary = FALSE,
        is_unique = FALSE
      ) |>
        dplyr::arrange(table_name, index_name),
      tolerance = 0.0001
    )
})
