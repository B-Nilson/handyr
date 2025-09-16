insert_file_lines <- function(file_path, line_number, lines_to_insert) {
  # Ensure sed is installed
  # TODO: add fallback that is not inline
  check_if_cmd_exists("sed")

  # Convert matrices to data.frames
  if (is.matrix(lines_to_insert)) {
    lines_to_insert <- as.data.frame(lines_to_insert)
  }

  # Convert data.frames to a vector of line strings
  if (is.data.frame(lines_to_insert)) {
    lines_to_insert <- lines_to_insert |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), \(x) paste0('"', x, '"'))
      ) |>
      tidyr::unite("line", dplyr::everything(), sep = ",", ) |> 
      dplyr::pull("line")
  }

  # Write to temp file
  temp_file <- tempfile()
  writeLines(lines_to_insert, temp_file)

  # Build and run command
  'sed -i "%sr %s" %s' |>
    sprintf(line_number - 1, temp_file, file_path) |> 
    system(intern = TRUE)
}
