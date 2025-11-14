# Create an Interval object from a date range

`Interval` objects represent a range of time and can be created using
`%--%` from the `lubridate` package. `as_interval()` makes this 'pipe
friendly', allowing you to create an `Interval` object from a vector of
2 dates, datetimes, or characters (formatted as "YYYY-MM-DD HH:MM:SS"
UTC time or "now" for the current time).

## Usage

``` r
as_interval(date_range = NULL, start = NULL, end = NULL)
```

## Arguments

- date_range:

  (Optional - required if `start` and `end` are not provided). A vector
  of 2 dates, datetimes, or characters (formatted as "YYYY-MM-DD
  HH:MM:SS" UTC time or "now" for the current time) representing the
  start and end time period. NA values will be replaced with 1970-01-01
  or "now" for the first and second values, respectively.

- start:

  (Optional - required if `date_range` is not provided). A date,
  datetime, or character (formatted as "YYYY-MM-DD HH:MM:SS" UTC time or
  "now" for the current time) representing the start of the date range.

- end:

  (Optional - required if `date_range` is not provided). A date,
  datetime, or character (formatted as "YYYY-MM-DD HH:MM:SS" UTC time or
  "now" for the current time) representing the end of the date range.

## Value

An `Interval` object representing the date range.
