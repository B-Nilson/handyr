# Creates a tempdir sqlite database with airquality data
init_airquality_sqlite_test <- function() {
  db_name <- "__test.sqlite"
  db_dir <- tempdir()
  db <- create_database(
    name = db_name,
    return_connection = TRUE,
    path = db_dir
  )
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = airquality,
      primary_keys = c("Month", "Day")
    )
  return(list(db) |> setNames(file.path(db_dir, db_name)))
}