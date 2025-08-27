#' Log a message for script progress tracking
#'
#' @param ... One or more character (or coercible to) values or vectors of messages.
#'  If more than one message is provided, they will be combined with `paste(collapse = sep)`
#' @param header A logical value indicating if the message should be formatted as a header ("|--- message ---|")
#'   Default is `FALSE`
#' @param time A logical value indicating if the current time should be prepended to the log message.
#'   Default is the opposite of `header`
#' @param time_format A character value indicating the format of the timestamp.
#'   See [base::strptime()] for formating details.
#'   Default is "%Y-%m-%d %H:%M:%S" (YYYY-MM-DD HH:MM:SS).
#' @param tz A character value indicating the time zone to use for the timestamp.
#'   Default is [base::Sys.timezone()].
#' @param quiet A logical value indicating if the message should not be printed using [base::message()].
#'   Default is `FALSE`.
#' @param sep A character value indicating the separator to use between messages when combined if multiple messages are provided.
#'   Default is " ".
#'
#' @return an invisible list with the timestamp (POSIXct), output message (character), and original message (character).
#'   If `header == TRUE` the return is wrapped with `list(.log_init = {...})` to aid in tracking for [summarise_logs()]
#'
#' @export
#'
#' @examples
#' logs <- log_step("My Awesome Script", time = FALSE, header = TRUE)
#' logs$step_1 <- log_step("Step 1...")
#' # Do something
#' logs$step_two <- log_step("Step-", 2, "...", sep = "")
#' # Do something else
#' logs$done <- log_step("Complete")
#' # Summarise run times and save log to file
#' summarise_logs(logs, save_to = tempfile())
log_step <- function(
  ...,
  header = FALSE,
  time = !header,
  time_format = "%Y-%m-%d %H:%M:%S",
  tz = Sys.timezone(),
  quiet = FALSE,
  sep = " "
) {
  messages <- as.character(unlist(list(...)))
  messages <- messages[!is.na(messages)]

  # Handle inputs
  stopifnot(is.character(messages), length(messages) > 0)
  stopifnot(is.logical(header), length(header) == 1)
  stopifnot(is.logical(time), length(time) == 1)
  stopifnot(is.character(time_format), length(time_format) == 1)
  stopifnot(is.character(tz), length(tz) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Join message with `sep` if multiple messages
  original_message <- paste(messages, collapse = sep)
  message <- original_message # modify a copy so we can return the original

  # Get current timestamp
  timestamp <- Sys.time() |>
    format(time_format, tz = tz)

  # Prepend timestamp if desired
  if (time) {
    message <- timestamp |>
      paste0(": ", message)
  }

  # Format as header if desired
  if (header) {
    n_dashes <- (nchar(message) * 4) |>
      clamp(range = c(0, 20)) # TODO: allow for user control
    line <- rep("-", n_dashes) |> paste(collapse = "")
    message <- paste0("|", line, "   ", message, "   ", line, "|")
  }

  # Print message for logging (if desired)
  if (!quiet) {
    message(message)
  }

  # Create log entry for `summarise_logs()`
  log_entry <- list(
    timestamp = as.POSIXct(timestamp, format = time_format, tz = tz),
    message = message,
    text = original_message,
    is_header = header,
    is_timestamped = time
  )

  # return list(log_entry) instead of log_entry if header
  if (header) {
    log_entry <- list(.log_init = log_entry)
  }

  invisible(log_entry)
}
