# Silence unwanted output

This function wraps `tryCatch` and suppresses some or all of the
following:

- errors

- warnings

- messages

- output from [`print()`](https://rdrr.io/r/base/print.html)

- output from [`cat()`](https://rdrr.io/r/base/cat.html)

- output from [`writeLines()`](https://rdrr.io/r/base/writeLines.html)

## Usage

``` r
silence(
  this_please,
  errors = TRUE,
  warnings = TRUE,
  messages = TRUE,
  output = FALSE
)
```

## Arguments

- this_please:

  Some code to run silently. If running multiple lines use `{....}`

- errors:

  A logical value indicating if errors should be silenced. Default is
  `TRUE`.

- warnings:

  A logical value indicating if warnings should be silenced. Default is
  `TRUE`.

- messages:

  A logical value indicating if messages should be silenced. Default is
  `TRUE`.

- output:

  A logical value indicating if output from
  [`print()`](https://rdrr.io/r/base/print.html),
  [`cat()`](https://rdrr.io/r/base/cat.html), and
  [`writeLines()`](https://rdrr.io/r/base/writeLines.html) should be
  silenced. Default is `TRUE`.

## Value

The output from `this_please`

## Examples

``` r
silence(1 / 0)
silence(warning("test"))
silence(message("test"))
silence(print("test"))
#> [1] "test"
silence(cat("test"))
#> test
silence(1 + "a")
```
