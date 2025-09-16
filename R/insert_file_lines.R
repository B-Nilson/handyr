#' Insert new lines into a text-file inline
#'
#' @description
#' Appends lines to a file (if `line_number = -1`), or uses sed to insert select lines from a matrix/data.frame/vector into a file inline -
#' faster that loading the file into memory,
#' then inserting the lines, and then writing the file back out.
#'
#' @param file_path The path to the file to modify.
#' @param line_number The line number to insert the data at.
#'   If -1, the data will be appended to the end of the file.
#'   Line numbers start at 1, which in most cases is the header line - ensure you don't insert the header by accident.
#' @param new_lines A character vector of lines to insert, OR a matrix or data frame containing the data to insert.
#'
#' @return Invisibly returns `TRUE` if the file was editted successfully.
#' @export
insert_file_lines <- function(file_path, line_number, new_lines) {
  stopifnot(is.character(file_path) & length(file_path) == 1)
  stopifnot(
    is.numeric(line_number),
    length(line_number) == 1,
    line_number >= 1 | line_number == -1
  )
  stopifnot(
    is.character(new_lines) | is.matrix(new_lines) | is.data.frame(new_lines)
  )
  # Ensure sed is installed if not appending
  if (line_number != -1) {
    # TODO: add fallback that is not inline
    check_if_cmd_exists("sed")
  }

  # Convert matrices to data.frames
  if (is.matrix(new_lines)) {
    new_lines <- as.data.frame(new_lines)
  }

  # Convert data.frames to a vector of line strings
  if (is.data.frame(new_lines)) {
    new_lines <- new_lines |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), \(x) paste0('"', x, '"'))
      ) |>
      tidyr::unite("line", dplyr::everything(), sep = ",", ) |>
      dplyr::pull("line")
  }

  # Append to end if -1, otherwise use sed to insert
  if (line_number == -1) {
    result <- new_lines |>
      write(file_path, sep = "\n", append = TRUE)
  } else {
    # Write to temp file
    temp_file <- tempfile()
    writeLines(new_lines, temp_file)

    # Build and run command
    result <- 'sed -i "%sr %s" %s' |>
      sprintf(line_number - 1, temp_file, file_path) |>
      system(intern = TRUE)
  }

  invisible(length(result) == 0)
}
