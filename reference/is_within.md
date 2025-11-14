# Check if a date is within an interval

This is a wrapper for the `%within%` operator from the `lubridate`
package. You can create an `Interval` object using
[`as_interval()`](https://b-nilson.github.io/handyr/reference/as_interval.md).

## Usage

``` r
is_within(dates, interval)
```

## Arguments

- dates:

  A vector of dates.

- interval:

  An `Interval` object representing the date range, or something that
  can be coerced to an `Interval` object using
  [`as_interval()`](https://b-nilson.github.io/handyr/reference/as_interval.md).

## Value

A logical vector indicating whether each date is within the interval.
