#' Insert new lines into a text-file inline
#'
#' @description
#' Uses sed to insert select lines from a matrix/data.frame/vector into a file inline -
#' faster that loading the file into memory,
#' then inserting the lines, and then writing the file back out.
#'
#' @param file_path The path to the file to modify.
#' @param line_number The line number to insert the data at. Line numbers start at 1, which in most cases is the header line - ensure you don't insert the header by accident.
#' @param lines_to_insert A character vector of lines to insert, OR a matrix or data frame containing the data to insert.
#'
#' @value Invisibly returns `TRUE` if the file was editted successfully.
#' @export
insert_file_lines <- function(file_path, line_number, lines_to_insert) {
  stopifnot(is.character(file_path) & length(file_path) == 1)
  stopifnot(is.numeric(line_number) & length(line_number) == 1)
  stopifnot(
    is.character(lines_to_insert) |
      is.matrix(lines_to_insert) |
      is.data.frame(lines_to_insert)
  )
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
  result <- 'sed -i "%sr %s" %s' |>
    sprintf(line_number - 1, temp_file, file_path) |>
    system(intern = TRUE)

  invisible(length(result) == 0)
}
