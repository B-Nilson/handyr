# Convert an Interval object to a data frame

Split an `Interval` object into a data frame with two columns: `start`
and `end`.

## Usage

``` r
# S3 method for class 'Interval'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  An `Interval` object.

- row.names:

  Passed to [`data.frame()`](https://rdrr.io/r/base/data.frame.html).
  Either NULL or a character vector giving the row names for the data
  frame. Missing values are not allowed.

- optional:

  Passed to [`data.frame()`](https://rdrr.io/r/base/data.frame.html). A
  single logical value, if `TRUE`, setting row names and converting
  column names (to syntactic names: see make.names) is optional.
  `!optional` is passed to `check.names` in
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html). Default is
  `FALSE`.

- ...:

  Additional arguments passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Value

A data frame with two columns: `start` and `end`.
