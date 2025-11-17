# Shorten a number to a string with a unit prefix (e.g. "1K", "10M", "100B")

Provided numbers will be divided by their highest met threshold and
rounded to the specified number of decimal places. These values will
then be converted to characters and appended with the appropriate
threshold symbol.

## Usage

``` r
shorten_number(
  x,
  decimals = 2,
  reverse = FALSE,
  thresholds = list(K = 1000, M = 1e+06, B = 1e+09, T = 1e+12)
)
```

## Arguments

- x:

  A vector of numeric values to shorten, or character values to lengthen
  (if `reverse = TRUE`).

- decimals:

  The number of decimal places to round `x` to if `reverse = FALSE`.
  Default is `2`.

- reverse:

  A logical indicating if the thresholds should be used in reverse
  order.

- thresholds:

  A named list of thresholds. The names of the list are used as the unit
  prefix. The values of the list must be powers of 10. Default is
  `list("K" = 1e3, "M" = 1e6, "B" = 1e9, "T" = 1e12)`.

## Value

A shortened string representation of `x` if `reverse = FALSE`, or a
numeric vector of lengthened `x` values if `reverse = TRUE`.

## Details

The reverse can be performed by setting `reverse = TRUE` and providing a
character vector to `x` (i.e. "1K" -\> 1000).

## Examples

``` r
x <- c(
   1,
   100,
   1000,
   10000,
   100000,
   1000000,
   10000000,
   100000000,
   1000000000,
   10000000000
 )
shorten_number(x)
#>  [1] "1"    "100"  "1K"   "10K"  "100K" "1M"   "10M"  "100M" "1B"   "10B" 

x <- c("1", "100", "1K", "10K", "100K", "1M", "10M", "100M", "1B", "10B")
shorten_number(x, reverse = TRUE)
#> Warning: NAs introduced by coercion
#>  [1] 1e+00 1e+02 1e+03 1e+04 1e+05 1e+06 1e+07 1e+08 1e+09 1e+10
```
