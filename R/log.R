#' Log a message with optional timestamp and formatting
#'
#' @param message A character value of the message to be logged
#' @param header A logical value indicating if the message should be formatted as a header
#' @param time A logical value indicating if the current time should be prepended to the log message
#' @param time_format A character value indicating the format of the timestamp
#' @param tz A character value indicating the time zone to use for the timestamp
#' @param quiet A logical value indicating if the message should be printed
#'
#' @return a character vector of the formatted message
#' @export
#' @examples
#' log("My Awesome Script", time = FALSE, header = TRUE)
#' log("Step 1...")
#' # Do something
#' log("Step 2...")
#' # Do something else
#' log("Complete")
# TODO: rename to log_step due to overlap with base::log()
log <- function(message, header = FALSE, time = !header, time_format = "%Y-%m-%d %H:%M:%S", tz = NULL, quiet = FALSE) {

  stopifnot(is.character(message))
  stopifnot(is.logical(header), length(header) == 1)
  stopifnot(is.logical(time), length(time) == 1)
  stopifnot(is.character(time_format), length(time_format) == 1)
  stopifnot(is.character(tz) | is.null(tz), length(tz) == 1 | is.null(tz))
  stopifnot(is.logical(quiet), length(quiet) == 1)

  message <- paste(message, collapse = " ")
  original <- message
  
  # Get current timestamp
  if (is.null(tz)) tz <- Sys.timezone()
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
