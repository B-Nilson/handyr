#' Check that a vector of two dates within a specified date range
#' 
#' @description
#' [check_date_range] is useful for checking if a date range is within a specified date range,
#' and provides quick ways to generate date ranges using "now" or "YYYY-MM-DD HH" format dates.
#'
#' @param date_range,within A vector of 1 or 2 dates, datetimes, or characters 
#' (formatted as "YYYY-MM-DD HH:MM:SS" with the same timezone as `tz` or "now" for the current time) representing the start and end of: an input `date_range` or a maximum allowed date range (`within`).
#' NA values will be replaced with 1970-01-01 or "now" for the first and second values, respectively.
#' Default for `date_range` is "now".
#' Default for `within` is c(NA, NA) - which is equivalent to c("1970-01-01 00", "now")
#' @param tz A character string representing the time zone to use for the output.
#' @param now_time_step A character string representing the time step to use when generating the current time if `within` is not fully specified. Passed to [lubridate::floor_date()].
#'
#' @return
#'   A length-2 vector of Dates or POSIXcts representing the start and end of a date range.
#' @export
check_date_range <- function(
  date_range = "now",
  within = c(NA, NA),
  tz = "UTC",
  now_time_step = "1 hours"
) {
  stopifnot(length(date_range) %in% 1:2)
  stopifnot(
    length(within) %in% 1:2,
    all(is.na(within)) |
      "Date" %in% class(within) |
      "POSIXct" %in% class(within) |
      is.character(within)
  )
  stopifnot(!is.na(lubridate::as.duration(now_time_step)))

  # Pass within to this function to allow for both to be flexible
  if (!all(is.na(within))) {
    original <- within
    within <- within[!is.na(within)] |> 
      check_date_range(tz = tz, now_time_step = now_time_step)
    within[is.na(original)] <- NA
  }

  # Handle single value date_range
  if (length(date_range) == 1) {
    date_range <- c(date_range, date_range)
  }

  # Handle character inputs
  now <- lubridate::now(tz = tz) |>
    lubridate::floor_date(now_time_step)
  if (is.character(date_range)) {
    # Handle "now"
    if ("now" %in% date_range) {
      date_range <- date_range |>
        swap(what = "now", with = now |> format("%F %T"))
    }

    # Convert to POSIXct
    date_range <- date_range |>
      lubridate::ymd_hms(tz = tz) |>
      silence(output = FALSE)
    if (all(is.na(date_range))) {
      stop(
        "If `date_range` is a character it must be either 'now' ",
        "or date/time strings matching the `tz` timezone with this format: YYYY-MM-DD HH:MM:SS"
      )
    }
  }

  # Handle NA's in within (no bound)
  if (any(is.na(within))) {
    if (is.na(within[1])) {
      within[1] <- lubridate::as_datetime(0, tz = tz)
    }
    if (is.na(within[2])) {
      within[2] <- now
    }
    within <- lubridate::as_datetime(within, tz = tz)
  }

  # Handle dates before min date allowed
  if (any(date_range < within[1])) {
    if (all(date_range < within[1])) {
      stop(
        "At least one date_range value must be on or after ",
        format(within[1], "%F %T %Z")
      )
    }
    warning(
      "`date_range` values must be on or after ",
      format(within[1], "%F %T %Z"),
      ".\n ",
      "Set the `date_range` to a period from this date onwards to stop this warning."
    )
    date_range[date_range < within[1]] <- within[1]
  }

  # Handle dates after max date allowed
  if (!is.na(within[2])) {
    if (any(date_range > within[2])) {
      if (all(date_range > within[2])) {
        stop(
          "At least one date_range value must be on or before ",
          format(within[2], "%F %T %Z")
        )
      }
      warning(
        "`date_range` values must be on or before ",
        format(within[2], "%F %T %Z"),
        "Set the max `date_range` to a period from ",
        "this date or earlier to stop this warning."
      )
      date_range[date_range > within[2]] <- within[2]
    }
  }

  # Ensure right time zones
  date_range |>
    lubridate::with_tz(tz)
}
