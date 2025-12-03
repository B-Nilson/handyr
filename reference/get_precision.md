# Get the number of decimal places in a numeric vector

Get the number of decimal places in a numeric vector

## Usage

``` r
get_precision(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A numeric vector with the same length as `x`, giving the number of
decimal places for each element in `x`.

## Examples

``` r
get_precision(1:5)
#> [1] 0 0 0 0 0
get_precision(c(1.2, 1.23, 1.234))
#> [1] 1 2 3
```
