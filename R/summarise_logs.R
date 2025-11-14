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
  logs_bound <- logs |>
    for_each(as.data.frame, .bind = TRUE, .quiet = TRUE) |>
    dplyr::mutate(.id = dplyr::row_number())
  log_summary <- logs_bound |>
    dplyr::group_by(is_not_section = .data$is_header | !.data$is_timestamped) |>
    dplyr::mutate(
      run_time = .data$timestamp |> dplyr::lead() |> difftime(.data$timestamp),
      run_time = dplyr::case_when(
        .data$is_not_section ~ NA,
        TRUE ~ .data$run_time
      )
    ) |>
    dplyr::group_by("run_time") |>
    dplyr::mutate(
      run_time_text = dplyr::case_when(
        .data$is_not_section ~ NA,
        TRUE ~
          .data$text |>
          paste0(
            ": ",
            .data$run_time |>
              as.numeric(units = "secs") |>
              prettyunits::pretty_sec()
          )
      )
    )

  # Extract run times for each section
  sections <- log_summary[
    !log_summary$is_not_section & log_summary$.id != max(log_summary$.id),
  ]
  total_time <- sum(sections$run_time, na.rm = TRUE) |>
    as.numeric(units = "secs") |>
    prettyunits::pretty_sec()

  # Log a summary of total time and each sections time
  time_summary <- log_step(
    "\nTotal time:",
    total_time,
    "\n-->",
    sections$run_time_text |> paste(collapse = "\n--> "),
    time = FALSE
  )

  # Combine log messages and time summary
  log_text <- log_summary$message |>
    c(time_summary$message |> strsplit(split = "\n") |> unlist())
  # Save log if file path provided
  if (!is.null(save_to)) {
    log_text |> writeLines(save_to)
  }
  return(invisible(log_summary))
}
