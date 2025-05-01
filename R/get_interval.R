get_interval <- function(x, most_common = TRUE, na.rm = FALSE) {
  # Handle inputs
  stopifnot(is.numeric(x) | inherits(x, "Date") | inherits(x, "POSIXt"))
  stopifnot(is.logical(na.rm), length(na.rm) == 1)

  # Handle NAs before calculating
  if (na.rm) x <- x[!is.na(x)]

  # Handle cases where get_interval doesn't need to be called
  if (length(x) == 0 | all(is.na(x))) {
    return(NA_real_)
  }

  # Get interval frequencies
  intervals <- diff(x)
  unique_intervals <- unique(intervals)
  frequencies <- match(intervals, unique_intervals) |>
    tabulate()

  # Combine into data frame and sort
  interval_freqs <- data.frame(
    interval = unique_intervals,
    frequency = frequencies
  )[order(-frequencies), ]

  # Return most common interval value
  if (most_common) {
    return(interval_freqs$interval[1])
  } else { # or data frame of all intervals and frequencies
    return(interval_freqs)
  }
}
