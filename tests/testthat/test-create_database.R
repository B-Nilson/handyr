test_that("SQLite works", {
  db_path <- create_database("__test.sqlite")
  expect_true(file.exists(db_path))
  file.remove(db_path)
})

test_that("duckdb works", {
  db_path <- create_database("__test.duckdb")
  expect_true(file.exists(db_path))
  file.remove(db_path)
})
