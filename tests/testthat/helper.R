# Creates a tempdir sqlite database with airquality data
init_airquality_db_test <- function(type = "sqlite") {
  db_name <- "test_tbl"
  db_dir <- tempdir()
  db_data <- datasets::airquality |>
    as.data.frame() |>
    dplyr::mutate(
      date = as.Date(paste0("2025-", Month, "-", Day)),
      datetime = as.POSIXct(date, tz = "UTC") + 1
    )

  db <- create_database(
    name = db_name,
    type = type,
    return_connection = TRUE,
    path = db_dir
  )
  db |>
    write_to_database(
      table_name = "airquality",
      new_data = db_data,
      primary_keys = c("Month", "Day")
    )
  return(
    list(db, db_data) |> 
      setNames(c(paste0(db_dir, "/", db_name, ".", type), "original"))
  )
}
