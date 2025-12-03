# Check that a vector of two dates within a specified date range

check_date_range is useful for checking if a date range is within a
specified date range, and provides quick ways to generate date ranges
using "now" or "YYYY-MM-DD HH" format dates.

## Usage

``` r
check_date_range(
  date_range = "now",
  within = c(NA, NA),
  tz = "UTC",
  now_time_step = "1 hours",
  as_interval = FALSE
)
```

## Arguments

- date_range, within:

  A vector of 1 or 2 dates, datetimes, or characters (formatted as
  "YYYY-MM-DD HH:MM:SS" with the same timezone as `tz` or "now"/"today"
  for the current time, or "yesterday" or "tomorrow" for that plus/minus
  one day) representing the start and end of: an input `date_range` or a
  maximum allowed date range (`within`). NA values will be replaced with
  1970-01-01 or "now" for the first and second values, respectively.
  Default for `date_range` is "now". Default for `within` is c(NA, NA) -
  which is equivalent to c("1970-01-01 00", "now")

- tz:

  A character string representing the time zone to use for the output.

- now_time_step:

  A character string representing the time step to use when generating
  the current time if `within` is not fully specified. Passed to
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html).

- as_interval:

  A logical value indicating whether to return an `Interval` object
  instead of a vector of dates.

## Value

A length-2 vector of Dates or POSIXcts representing the start and end of
a date range.
