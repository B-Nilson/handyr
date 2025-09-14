test_that("SQLite works", {
  db_path <- create_database(name = "__test.sqlite", return_connection = FALSE)
  expect_true(file.exists(db_path))
  file.remove(db_path)
})

test_that("duckdb works", {
  db_path <- create_database("__test.duckdb", return_connection = FALSE)
  expect_true(file.exists(db_path))
  file.remove(db_path)
})

test_that("postgresql works", {
  skip("Skipping PostgreSQL tests as they download/invoke external binaries")
  db <- create_database("__test.postgresql")
  expect_true(DBI::dbIsValid(db))
  DBI::dbDisconnect(db)
})

test_that("setting path works", {
  db_path <- create_database("__test.sqlite", path = tempdir(), return_connection = FALSE)
  expect_true(file.exists(db_path))
  file.remove(db_path)
})

test_that("setting type works", {
  db_path <- create_database("__test.sqlite", type = "duckdb", return_connection = FALSE)
  expect_true(file.exists(db_path))
  expect_equal(tools::file_ext(db_path), "duckdb")
  file.remove(db_path)
})