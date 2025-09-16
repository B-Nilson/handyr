test_that("basic case works", {
  # Write out a test datafile
  data_file_path <- tempfile()
  test_data <- datasets::airquality
  row.names(test_data) <- NULL
  test_data |> write.csv(data_file_path, row.names = FALSE)

  # Remove first 5 lines (excluding header)
  data_file_path |>
    delete_file_lines(line_range = c(2, 6))

  # Check that worked
  read.csv(data_file_path) |>
    expect_equal(
      test_data |>
        dplyr::slice(-c(1:5)),
      tolerance = 0.0001
    )
  
  # Cleanup
  file.remove(data_file_path)
})
