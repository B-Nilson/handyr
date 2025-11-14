#' Create an Interval object from a date range
#' @description
#' `Interval` objects represent a range of time and  can be created using `%--%` from the `lubridate` package.
#' [as_interval()] makes this 'pipe friendly', allowing you to create an `Interval` object from a vector of 2 dates, datetimes, or characters
#' (formatted as "YYYY-MM-DD HH:MM:SS" UTC time or "now" for the current time).
#'
#' @param date_range (Optional - required if `start` and `end` are not provided).
#'   A vector of 2 dates, datetimes, or characters
#'   (formatted as "YYYY-MM-DD HH:MM:SS" UTC time or "now" for the current time) representing the start and end time period.
#'   NA values will be replaced with 1970-01-01 or "now" for the first and second values, respectively.
#' @param start (Optional - required if `date_range` is not provided).
#'   A date, datetime, or character (formatted as "YYYY-MM-DD HH:MM:SS" UTC time or "now" for the current time) representing the start of the date range.
#' @param end (Optional - required if `date_range` is not provided).
#'   A date, datetime, or character (formatted as "YYYY-MM-DD HH:MM:SS" UTC time or "now" for the current time) representing the end of the date range.
#' @return An `Interval` object representing the date range.
#' @export
as_interval <- function(date_range = NULL, start = NULL, end = NULL) {
  stopifnot(
    "At least one of `date_range` or `start` and `end` must be provided." = !is.null(
      date_range
    ) |
      !is.null(start) & !is.null(end)
  )
  # Handle date_range provided instead of start/end
  if (!is.null(date_range)) {
    start <- date_range[[1]]
    end <- date_range[[2]]
  }

  # Handle non-date start/end
  if (
    "character" %in%
      class(start) |
      "character" %in% class(end) |
      any(is.na(c(start, end)))
  ) {
    # TODO: vectorise check_date_range()
    date_ranges <- start |>
      for_each(
        .enumerate = TRUE,
        .bind = TRUE,
        .show_progress = FALSE,
        \(value, i) {
          check_date_range(c(start = value, end = end[[i]])) |>
            as.list() |>
            as.data.frame()
        }
      )
    start <- date_ranges[[1]]
    end <- date_ranges[[2]]
  }
  lubridate::`%--%`(start, end)
}

#' Extend seq() to work with Intervals
#'
#' @description
#' `Interval` objects can be created using `%--%` from the `lubridate` package, or the handy wrapper [as_interval()].
#' These objects represent a range of time, so it makes sense to be able to generate a sequence of dates within that range.
#'
#' [seq.Interval()] extends [seq()] to work with `Interval` objects.
#'
#' @param interval A `Interval` object representing the date range.
#' @param by A string indicating the interval to use.
#'   Can be one of "auto", "1 seconds", "1 minutes", "1 hours", or "1 days".
#'   If "auto", the interval will be automatically determined based length of the interval.
#'   Default is "auto".
#' @param ... Additional arguments passed to `seq()`.
#'
#' @return A sequence of dates within the interval.
#' @export
seq.Interval <- function(interval, by = "auto", ...) {
  if (by == "auto") {
    interval_length <- lubridate::int_length(interval)
    interval_thresholds <- c(
      "1 seconds" = 60,
      "1 minutes" = 3600,
      "1 hours" = 86400,
      "1 days" = 31536000,
      "1 years" = Inf
    )
    highest_passed_threshold <- max(which(
      interval_thresholds < interval_length
    ))
    by <- names(interval_thresholds)[highest_passed_threshold]
  }
  seq(
    from = lubridate::int_start(interval),
    to = lubridate::int_end(interval),
    by = by,
    ...
  )
}

#' Convert an Interval object to a data frame
#'
#' @description
#' Split an `Interval` object into a data frame with two columns: `start` and `end`.
#'
#' @param x An `Interval` object.
#' @param row.names Not used.
#' @param optional Not used.
#' @param ... Additional arguments passed to `data.frame()`.
#' @return A data frame with two columns: `start` and `end`.
#' @export
as.data.frame.Interval <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    start = lubridate::int_start(x),
    end = lubridate::int_end(x),
    ...
  )
}
