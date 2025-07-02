#' Split a date range into multiple, shorter date ranges
#'
#' @param date_range A length-2 vector of Dates or POSIXcts representing the start and end of a date range.
#' @param max_duration A string representing the maximum duration of each split date range. Defaults to "90 days".
#' @param as_list A logical indicating if the output should be a list of data frames (one for each split date range) or a single data frame with all split date ranges. Defaults to `FALSE`.
#'
#' @return
#'   If `as_list = FALSE` (default), a data frame with columns `start` and `end` representing the start and end of each split date range.
#'   If `as_list = TRUE`, a list of data frames, each with columns `start` and `end` representing the start and end of each split date range.
#'
#' @export
split_date_range <- function(
    date_range,
    max_duration = "90 days",
    as_list = FALSE) {
  starts <- date_range[1] |> seq(date_range[2], by = max_duration)
  ends <- starts |> dplyr::lead(default = date_range[2])
  combined <- data.frame(start = starts, end = ends)

  if (as_list) {
    combined <- combined |>
      dplyr::rowwise() |>
      dplyr::group_split()
  }
  return(combined)
}
