test_that("basic case works", {
  # Write out a test datafile
  data_file_path <- tempfile()
  test_data <- datasets::airquality
  row.names(test_data) <- NULL
  test_data |> write.csv(data_file_path, row.names = FALSE)

  # Insert 5 extra lines just after header
  new_lines <- test_data |> 
    utils::tail(5) |> 
    dplyr::mutate(Month = 12)
  
  data_file_path |>
    insert_file_lines(line_number = 2, lines_to_insert = new_lines)

  # Check that worked
  expected <- new_lines |> rbind(test_data) |> as.matrix()
  row.names(expected) <- NULL
  read.csv(data_file_path) |>
    as.matrix() |> 
    expect_equal(expected, tolerance = 0.0001)
  
  # Cleanup
  file.remove(data_file_path)
})
