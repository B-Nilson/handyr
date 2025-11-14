# Extend seq() to work with Intervals

`Interval` objects can be created using `%--%` from the `lubridate`
package, or the handy wrapper
[`as_interval()`](https://b-nilson.github.io/handyr/reference/as_interval.md).
These objects represent a range of time, so it makes sense to be able to
generate a sequence of dates within that range.

`seq.Interval()` extends [`seq()`](https://rdrr.io/r/base/seq.html) to
work with `Interval` objects.

## Usage

``` r
# S3 method for class 'Interval'
seq(interval, by = "auto", ...)
```

## Arguments

- interval:

  A `Interval` object representing the date range. Multiple intervals
  will be looped over using
  [`sapply()`](https://rdrr.io/r/base/lapply.html) with the same `by`
  and `...` arguments.

- by:

  A string indicating the interval to use. Can be one of "auto", "1
  seconds", "1 minutes", "1 hours", or "1 days". If "auto", the interval
  will be automatically determined based length of the interval. Default
  is "auto".

- ...:

  Additional arguments passed to
  [`seq()`](https://rdrr.io/r/base/seq.html).

## Value

A sequence of dates within the interval.
