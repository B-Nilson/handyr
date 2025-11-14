# Split a date range into multiple, shorter date ranges

Split a date range into multiple, shorter date ranges

## Usage

``` r
split_date_range(date_range, max_duration = "90 days", as_list = FALSE)
```

## Arguments

- date_range:

  A length-2 vector of Dates or POSIXcts representing the start and end
  of a date range.

- max_duration:

  A string representing the maximum duration of each split date range.
  Defaults to "90 days".

- as_list:

  A logical indicating if the output should be a list of data frames
  (one for each split date range) or a single data frame with all split
  date ranges. Defaults to `FALSE`.

## Value

If `as_list = FALSE` (default), a data frame with columns `start` and
`end` representing the start and end of each split date range. If
`as_list = TRUE`, a list of data frames, each with columns `start` and
`end` representing the start and end of each split date range.
