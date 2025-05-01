#' Summarise logs stored from repeated `log` calls
#'
#' @param logs A list of `log` values
#'
#' @description
#' `summarise_logs` takes a list of `log` values and returns a summary of the time taken between each log call.
#'
#' @family Utilities
#'
#' @return a character vector indicating the summary of the time taken between each log call
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' logs <- list(
#'   log_step("test", quiet = TRUE),
#'   log_step("test", quiet = TRUE),
#'   log_step("test", quiet = TRUE),
#'   log_step("test", quiet = TRUE),
#'   log_step("test", quiet = TRUE),
#'   log_step("test", quiet = TRUE)
#' )
#' summarise_logs(logs)
summarise_logs <- function(logs) {
  # Determine run times
  sections <- logs |>
    for_each(as.data.frame, .bind = TRUE) |>
    dplyr::mutate(
      run_time = .data$timestamp |> dplyr::lead() |> difftime(.data$timestamp),
      units = attr(.data$run_time, "units"),
      text = .data$text |>
        paste0(": ", .data$run_time, " ", .data$units)
    )
  sections <- sections[-c(1, nrow(sections)), ]

  total_time <- sum(sections$run_time, na.rm = TRUE)
  time_units <- total_time |> attr("units")

  message(
    paste(
      "Total time:", total_time, time_units,
      "\n-->", sections$text |>
        paste(collapse = "\n--> ")
    )
  )
}
