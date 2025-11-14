# Replace out-of-range values with the nearest in-range value

`clamp` sets values less than `min(range)` to `min(range)` and values
greater than `max(range)` to `max(range)`

## Usage

``` r
clamp(x, range = c(NA, NA))
```

## Arguments

- x:

  A numeric vector.

- range:

  A numeric vector of length two to clamp `x` to.

  - If `range[1]` is `NA` (default), then `min(x)` will be used.

  - If `range[2]` is `NA` (default), then `max(x)` will be used.

## Value

A numeric vector equal to `x` with:

- values less than `min(range)` set to `min(range)`

- values greater than `max(range)` set to `max(range)`

## Examples

``` r
clamp(1:5, range = c(2, NA)) # 2, 2, 3, 4, 5
#> [1] 2 2 3 4 5
clamp(1:5, range = c(2, 4)) # 2, 2, 3, 4, 4
#> [1] 2 2 3 4 4
clamp(1:5, range = c(-1, 1)) # 1, 1, 1, 1, 1
#> [1] 1 1 1 1 1
```
