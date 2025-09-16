delete_file_lines <- function(file_path, line_range) {
  # Handle edge case where no lines desired to be removed
  if (line_range[1] == line_range[2]) {
    return(invisible(FALSE))
  }

  # Ensure sed is installed
  # TODO: add fallback that is not inline
  sed_exists <- check_if_cmd_exists("sed")
  if (!sed_exists) {
    stop("`sed` is not installed")
  }

  # Use sed (i=inline) to delete select range
  result <- "sed -i '%d,%dd' '%s'" |>
    sprintf(line_range[1], line_range[2], file_path) |> 
    system(intern = TRUE)
  invisible(length(result) == 0)
}

check_if_cmd_exists <- function(cmd) {
  system(paste(cmd, "--help"), intern = TRUE) |>
    is.character() |>
    on_error(.return = FALSE)
}
