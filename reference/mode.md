# Get the most common value

`mode` determines the statistical mode (the most common value) of one or
more vectors

## Usage

``` r
mode(..., na.rm = FALSE)
```

## Arguments

- ...:

  numeric or character vectors (see base::max).

- na.rm:

  A logical value indicating whether NA values should be removed.

## Value

A single value with the same type as `...`

## Examples

``` r
mode(c("A", "B", "A")) # Returns "A"
#> [1] "A"
mode(1:3, 3:6, 2:4, NA) # Returns 3
#> [1] 3
mode(1, 1:3, c(NA, NA, NA)) # Returns NA
#> [1] NA
mode(1, 1:3, c(NA, NA, NA), na.rm = TRUE) # Returns 1
#> [1] 1
```
