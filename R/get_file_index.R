#' Get a file index from a URL
#'
#' This function fetches a file index from a URL for a remote directory.
#' The file index is a data frame with columns for the file/directory
#' name, last modified date, size, and type (file or dir).
#'
#' @param url The URL to fetch the file index from
#' @param size_unit The unit to express the file sizes in. Defaults to "kB".
#' @param timeout The timeout in seconds for the httr request. Defaults to 2.
#'
#' @export
#' @examples
#' get_file_index("https://aqmap.ca/aqmap/outputs/")
get_file_index <- function(url, size_unit = "kB", timeout = 2) {
  # Cleanup unit definition if needed
  if (1000 |> convert_units(from = "bytes", to = "kB") != 1) {
    units::remove_unit("B")
    units::install_unit("B", "byte")
  }

  # Handle inputs
  stopifnot(is.character(url), length(url) == 1)
  stopifnot(is.character(size_unit), length(size_unit) == 1)
  stopifnot(
    "`size_unit` must be a valid unit that can be converted from bytes (e.g. 'kB', 'MB', 'GB', etc.)" = 1 |>
      convert_units(from = "B", to = size_unit) |>
      on_error(.return = FALSE) >
      0
  )
  stopifnot(is.numeric(timeout), length(timeout) == 1)

  # Define file size symbols used in file index
  file_size_symbols <- list(
    "K" = 1e3,
    "M" = 1e6,
    "G" = 1e9,
    "T" = 1e12,
    "P" = 1e15
  )

  # Try to get file index
  response <- url |>
    httr::GET(httr::timeout(timeout)) |>
    on_error(.return = NULL)
  if (is.null(response)) {
    stop(
      "Timeout reached - ensure url is actually a file index, and increase `timeout` if needed."
    )
  } else if (httr::status_code(response) != 200) {
    stop(
      "Failed to fetch file index - status code ",
      httr::status_code(response),
      ". Check url and try again."
    )
  }
  response_content <- response |> httr::content()

  # Extract tables from response
  page_tables <- response_content |> rvest::html_table()
  if (
    length(page_tables) != 1 ||
      on_error(ncol(page_tables[[1]]) != 5, .return = FALSE)
  ) {
    if (on_error(ncol(page_tables[[1]]) != 5, .return = TRUE)) {
      stop(
        "Provided url appears to not be a file index ",
        "(maybe it redirects to an index.html?). Check url and try again."
      )
    }
  }

  # Extract links (the tail-end are the file/dir names)
  table_links <- response_content |>
    rvest::html_element("table") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  # Reformat file index (first table for a valid file index)
  page_tables[[1]] |>
    dplyr::select(2:4) |>
    stats::setNames(c("name", "last_modified", "size")) |>
    dplyr::filter(.data$last_modified != "") |>
    dplyr::mutate(
      # Replace shotened names with full names
      name = table_links |> utils::tail(dplyr::n()),
      # Add type (file or dir)
      type = .data$name |>
        endsWith("/") |>
        ifelse(yes = "dir", no = "file") |>
        factor(),
      # Format last modified to datetime
      last_modified = .data$last_modified |>
        lubridate::ymd_hm(tz = lubridate::tz(Sys.time())),
      # Format size to desired unit
      size = .data$size |>
        shorten_number(reverse = TRUE, thresholds = file_size_symbols) |>
        convert_units(from = "B", to = size_unit, keep_units = TRUE)
    )
}
