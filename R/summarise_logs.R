#' Log overall and individual run time for repeated [log_step()] calls
#'
#' `summarise_logs` takes a list of log entries from [log_step()] and outpus a message with a summary of the time taken between each log call.
#'
#' @param logs A list of [log_step()] return values
#' @param save_to A character value indicating the path to a text file to save the summary of the logs to.
#'   Default is `NULL` (do not save a log file).
#'
#' @family Utilities
#'
#' @return a character vector indicating the summary of the time taken between each log call
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' logs <- log_step("Example usage of log_summarise", header = TRUE)
#' logs$step_one <- log_step("Step-1...")
#' # Do something
#' logs$step_two <- log_step("Step-2...")
#' # Do something else
#' logs$step_three <- log_step("Step-3...")
#' # Do something else
#' logs$done <- log_step("Complete")
#' # Summarise run times and write to file
#' summarise_logs(logs, save_to = tempfile())
summarise_logs <- function(logs, save_to = NULL) {
  stopifnot(is.list(logs), length(logs) > 2)
  stopifnot(
    is.character(save_to) | is.null(save_to),
    length(save_to) == 1 | is.null(save_to)
  )

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
