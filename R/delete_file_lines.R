#' Delete select lines from a file inline
#' 
#' @description
#' Uses sed to delete select lines from a file inline - 
#' faster that loading the file into memory, 
#' then deleting the lines, and then writing the file back out.
#'
#' @param file_path The path to the file to modify.
#' @param line_range A numeric vector of length 2 indicating the start and end of the range of lines to delete. Line numbers start at 1, which in most cases is the header line - ensure you don't delete the header by accident.
#' @export
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
