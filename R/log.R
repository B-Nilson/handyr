#' Log a message with optional timestamp and formatting
#'
#' @param message A character value of the message to be logged
#' @param time A logical value indicating if the current time should be prepended to the log message
#' @param time_format A character value indicating the format of the timestamp
#' @param tz A character value indicating the time zone to use for the timestamp
#' @param header A logical value indicating if the message should be formatted as a header
#' @param quiet A logical value indicating if the message should be printed
#'
#' @return a character vector of the formatted message
#' @export
#' @examples
#' log("My Awesome Script", time = FALSE, header = TRUE)
#' log("Step 1...")
#' Sys.sleep(0.8)
#' log("Step 2...")
#' log("Complete", time = FALSE, header = TRUE)
log <- function(message, time = TRUE, time_format = "%Y-%m-%d %H:%M:%S", tz = NULL, header = FALSE, quiet = FALSE) {
  original <- message
  # Get current timestamp
  if (is.null(tz)) tz <- Sys.timezone()
  timestamp <- Sys.time() |>
    format(time_format, tz = tz)

  # Prepend timestamp if desired
  if (time) message <- paste0(timestamp, ": ", message)

  # Format as header if desired
  if (header) {
    n_dashes <- clamp(nchar(message) * 4, c(0, 20))
    line <- rep("-", n_dashes) |> paste(collapse = "")
    message <- paste0("|", line, "   ", message, "   ", line, "|")
  }

  # Print message (if desired) and return list for tracking
  if (!quiet) message(message)
  invisible(
    list(
      timestamp = as.POSIXct(timestamp, format = time_format, tz = tz),
      message = message,
      text = original
    )
  )
}
