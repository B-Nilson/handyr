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

test_that("partitioning works for sqlite", {
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "sqlite")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  withr::defer({
    DBI::dbDisconnect(db)
    file.remove(db_path)
  })

  # Make dataset to insert
  new_data <- mtcars |>
    as.data.frame() |>
    dplyr::mutate(car = row.names(mtcars)) |>
    dplyr::arrange(car)
  row.names(new_data) <- NULL

  # Create partitioned table
  db |>
    create_database_table(
      table_name = "mtcars",
      new_data = new_data,
      primary_keys = c("car"),
      partition_by = list(gear = list(c(1, 4), c(4, 6)))
    )

  # Check partitions are created
  expected_tables <- paste0("mtcars_gear_", c(1, 4), "to", c(4, 6))
  db |>
    DBI::dbListTables() |>
    expect_contains(expected_tables)

  # Check View exists
  db |>
    DBI::dbGetQuery(
      "SELECT name FROM sqlite_master WHERE type = 'view' AND name = 'mtcars'"
    ) |>
    dplyr::pull(name) |>
    expect_equal("mtcars")

  # Check view data is correct
  db |>
    DBI::dbGetQuery("SELECT * FROM mtcars") |>
    dplyr::arrange(car) |>
    expect_equal(
      new_data,
      tolerance = 0.0001
    )

  # Check each partition is correct
  new_data |>
    dplyr::group_split(
      partition_name = (gear < 4) |>
        factor(
          levels = c(TRUE, FALSE),
          labels = paste0("mtcars_gear_", c(1, 4), "to", c(4, 6))
        )
    ) |>
    lapply(\(partition_data) {
      db |>
        dplyr::tbl(as.character(partition_data$partition_name[1])) |>
        dplyr::arrange(car) |>
        dplyr::collect() |>
        expect_equal(
          partition_data |> dplyr::select(-partition_name),
          tolerance = 0.0001
        )
    }) |>
    expect_no_warning() |>
    expect_no_error()
})

test_that("partitioning works for duckdb", {
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
    dplyr::mutate(car = row.names(mtcars)) |>
    dplyr::arrange(car)
  row.names(new_data) <- NULL

  # Create partitioned table
  db |>
    create_database_table(
      table_name = "mtcars",
      new_data = new_data,
      primary_keys = c("car"),
      partition_by = list(gear = list(c(1, 4), c(4, 6)))
    )

  # Check partitions are created
  expected_tables <- paste0("mtcars_gear_", c(1, 4), "to", c(4, 6))
  db |>
    DBI::dbGetQuery("SELECT * FROM duckdb_tables()") |>
    dplyr::pull(table_name) |>
    expect_contains(expected_tables)

  # Check View exists
  db |>
    DBI::dbGetQuery(
      "SELECT * FROM duckdb_views() WHERE view_name = 'mtcars'"
    ) |>
    dplyr::pull(view_name) |>
    expect_equal("mtcars")

  # Check view data is correct
  db |>
    DBI::dbGetQuery("SELECT * FROM mtcars") |>
    dplyr::arrange(car) |>
    expect_equal(
      new_data,
      tolerance = 0.0001
    )

  # Check each partition is correct
  new_data |>
    dplyr::group_split(
      partition_name = (gear < 4) |>
        factor(
          levels = c(TRUE, FALSE),
          labels = paste0("mtcars_gear_", c(1, 4), "to", c(4, 6))
        )
    ) |>
    lapply(\(partition_data) {
      db |>
        dplyr::tbl(as.character(partition_data$partition_name[1])) |>
        dplyr::arrange(car) |>
        dplyr::collect() |>
        expect_equal(
          partition_data |> dplyr::select(-partition_name),
          tolerance = 0.0001
        )
    }) |>
    expect_no_warning() |>
    expect_no_error()
})

test_that("partitioning works for postgres", {
  skip("Skipping PostgreSQL tests as they download/invoke external binaries")
  # Create temp db to work with
  db_list <- init_airquality_db_test(type = "postgresql")
  db_path <- names(db_list)[1]
  db <- db_list[[1]]
  withr::defer({
    db |> DBI::dbRemoveTable("mtcars") |> on_error(.return = NULL)
    DBI::dbDisconnect(db)
  })

  # Make dataset to insert
  new_data <- mtcars |>
    as.data.frame() |>
    dplyr::mutate(car = row.names(mtcars)) |>
    dplyr::arrange(car)
  row.names(new_data) <- NULL

  # Create partitioned table
  db |>
    create_database_table(
      table_name = "mtcars",
      new_data = new_data,
      primary_keys = c("car", "gear"), # TODO: automate inclusion of partition key in primary keys if not present
      partition_by = list(gear = list(c(1, 4), c(4, 6)))
    )

  # Check partitions are created
  expected_tables <- paste0("mtcars_gear_", c(1, 4), "to", c(4, 6))
  db |>
    DBI::dbListTables() |>
    expect_contains(expected_tables)

  # Check partitioned data is correct
  db |>
    DBI::dbGetQuery("SELECT * FROM mtcars") |>
    dplyr::arrange(car) |>
    expect_equal(
      new_data,
      tolerance = 0.0001
    )

  # Check each partition is correct
  new_data |>
    dplyr::group_split(
      partition_name = (gear < 4) |>
        factor(
          levels = c(TRUE, FALSE),
          labels = paste0("mtcars_gear_", c(1, 4), "to", c(4, 6))
        )
    ) |>
    lapply(\(partition_data) {
      db |>
        dplyr::tbl(as.character(partition_data$partition_name[1])) |>
        dplyr::arrange(car) |>
        dplyr::collect() |>
        expect_equal(
          partition_data |> dplyr::select(-partition_name),
          tolerance = 0.0001
        )
    }) |>
    expect_no_warning() |>
    expect_no_error()
})
