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
summarise_logs <- function(logs, save_to = NULL) {
  # Make log data frame with run times
  log_summary <- logs |>
    for_each(as.data.frame, .bind = TRUE) |>
    dplyr::mutate(
      run_time = .data$timestamp |> dplyr::lead() |> difftime(.data$timestamp),
      units = attr(.data$run_time, "units"),
      text = .data$text |>
        paste0(": ", .data$run_time, " ", .data$units)
    )
  
  # Extract run times for each section
  sections <- log_summary[-c(1, nrow(log_summary)), ]
  total_time <- sum(sections$run_time, na.rm = TRUE)
  time_units <- total_time |> attr("units")

  # Log a summary of total time and each sections time
  time_summary <- log_step(
    "\nTotal time:", total_time, time_units,
    "\n-->", sections$text |> paste(collapse = "\n--> "),
    time = FALSE
  )

  # Combine log messages and time summary
  log_text <- log_summary$message |>
    c(time_summary$message |> strsplit(split = "\n") |> unlist())
  # Save log if file path provided
  if (!is.null(save_to)) {
    log_text |> writeLines(save_to)
  }
  return(invisible(log_text))
}
