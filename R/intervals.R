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

#' Check if a date is within an interval
#'
#' This is a wrapper for the `%within%` operator from the `lubridate` package.
#' You can create an `Interval` object using [as_interval()].
#'
#' @param dates A vector of dates.
#' @param interval An `Interval` object representing the date range, or something that can be coerced to an `Interval` object using [as_interval()].
#' @return A logical vector indicating whether each date is within the interval.
#' @export
is_within <- function(dates, interval) {
  if (!inherits(interval, "Interval")) {
    interval <- as_interval(interval)
  }
  stopifnot(length(interval) == 1)
  lubridate::`%within%`(dates, interval)
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
#'   Multiple intervals will be looped over using `sapply()` with the same `by` and `...` arguments.
#' @param by A string indicating the interval to use.
#'   Can be one of "auto", "1 seconds", "1 minutes", "1 hours", or "1 days".
#'   If "auto", the interval will be automatically determined based length of the interval.
#'   Default is "auto".
#' @param ... Additional arguments passed to `seq()`.
#'
#' @return A sequence of dates within the interval.
#' @export
seq.Interval <- function(interval, by = "auto", ...) {
  if (length(interval) > 1) {
    return(lapply(interval, seq, by = by, ...))
  }
  stopifnot("`interval` must have a length of 1." = length(interval) == 1)
  stopifnot(
    "`by` must be a character vector of length 1." = inherits(by, "character") &
      length(by) == 1
  )
  stopifnot(
    "`by` must either be 'auto' or a period interpretable by `lubridate::as.period()` (e.g. '1 hours')." = by ==
      "auto" |
      !is.na(lubridate::as.period(by))
  )
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
#' @param row.names Passed to `data.frame()`.
#'   Either NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Passed to `data.frame()`.
#'   A single logical value, if `TRUE`, setting row names and converting column names (to syntactic names: see make.names) is optional.
#'   `!optional` is passed to `check.names` in `data.frame()`.
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to `data.frame()`.
#' @return A data frame with two columns: `start` and `end`.
#' @export
as.data.frame.Interval <- function(x, row.names = NULL, optional = FALSE, ...) {
  stopifnot("At least one interval must be provided." = length(x) >= 1)
  stopifnot(
    "`row.names` must be NULL or a character vector with no missing values." = inherits(
      row.names,
      "character"
    ) ||
      (is.null(row.names) & !any(is.na(row.names)))
  )
  stopifnot(
    "`optional` must be a single logical value." = is.logical(optional) &
      length(optional) == 1
  )

  data.frame(
    start = lubridate::int_start(x),
    end = lubridate::int_end(x),
    row.names = row.names,
    check.names = !optional,
    ...
  )
}
