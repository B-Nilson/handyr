#' Determine distance(s) between values
#'
#' `get_interval` finds the most common interval in a vector. If there are multiple intervals of the same frequency, it takes the first one in the sorted vector and a warning is raised.
#'
#' @param x A numeric, Date, POSIXt, or POSIXct vector.
#' @param most_common A logical value indicating whether the most common interval (default) should be returned or all intervals and their frequencies in a sorted data frame.
#' @param na.rm A logical value indicating whether NAs should be removed before calculating the interval frequencies.
#'   Default is `FALSE`.
#' @param quiet A logical value indicating whether warnings should be suppressed and output should be invisible.
#'   Default is `FALSE`.
#'
#' @return
#' * If `most_common = TRUE` (default), a single numeric or difftime value.
#' * If `most_common = FALSE`, a sorted data frame with columns `interval` (numeric or difftime) and `frequency` (integer).
#'
#' @export
#'
#' @examples
#' get_interval(c(1, 3, 5:10))
#' get_interval(as.Date(c("2020-01-01", "2020-01-03", "2020-01-05", "2020-01-06")))
#' get_interval(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:02", "2020-01-01 00:00:04")))
#' get_interval(c(0, 3, 5, 7:10), most_common = FALSE)
get_interval <- function(x, most_common = TRUE, na.rm = FALSE, quiet = FALSE) {
  # Handle inputs
  stopifnot(is.numeric(x) | inherits(x, c("Date", "POSIXt", "POSIXct")))
  stopifnot(is.logical(na.rm), length(na.rm) == 1)

  # Handle NAs before calculating
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Handle cases where get_interval doesn't need to be called
  if (length(x) == 0 | all(is.na(x))) {
    return(NA_real_)
  }

  # Get interval frequencies
  intervals <- diff(x)
  if (is.numeric(intervals)) {
    intervals <- round(intervals, digits = 14)
  } # handle floating point errors
  unique_intervals <- unique(intervals)
  frequencies <- match(intervals, unique_intervals) |>
    tabulate()

  # Combine into data frame and sort
  interval_freqs <- data.frame(
    interval = unique_intervals[order(-frequencies)],
    frequency = frequencies[order(-frequencies)]
  )

  # Warn if more than one interval has the same frequency
  if (!quiet & nrow(interval_freqs) > 1) {
    if (interval_freqs$frequency[1] == interval_freqs$frequency[2]) {
      warning(paste0(
        "More than one interval has the same top frequency.",
        if (most_common) " Returning the first interval.",
        if (most_common) {
          "\nSet `most_common = FALSE` to see all intervals and their frequencies."
        }
      ))
    }
  }

  # Return most common interval value
  if (most_common) {
    interval <- interval_freqs$interval[1]
    if (quiet) invisible(interval) else interval
  } else {
    # or data frame of all intervals and frequencies
    if (quiet) invisible(interval_freqs) else interval_freqs
  }
}
