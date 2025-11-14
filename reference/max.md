# Get the maximum/minimum value while handling NAs cleanly

`max`/`min` provides a wrapper for the built-in functions
base::max/base::min that handles NAs the same as
[base::mean](https://rdrr.io/r/base/mean.html).

## Usage

``` r
max(..., na.rm = FALSE)

min(..., na.rm = FALSE)
```

## Arguments

- ...:

  numeric or character vectors (see base::max).

- na.rm:

  A logical value indicating whether NA values should be removed.

## Value

A single value with the same type as `...`

## Details

This handles the case where all values provided to max/min are NA, which
normally returns Inf/-Inf and throws a warning (see examples).

## Examples

``` r
# Edge case where all values are NA
base::max(c(NA, NA, NA)) # Returns NA
#> [1] NA
base::max(c(NA, NA, NA), na.rm = TRUE) # Returns -Inf and throws warning
#> Warning: no non-missing arguments to max; returning -Inf
#> [1] -Inf
base::min(c(NA, NA, NA), na.rm = TRUE) # Returns Inf and throws warning
#> Warning: no non-missing arguments to min; returning Inf
#> [1] Inf
max(c(NA, NA, NA)) # Returns NA
#> [1] NA
max(c(NA, NA, NA), na.rm = TRUE) # Returns NA
#> [1] NA
min(c(NA, NA, NA), na.rm = TRUE) # Returns NA
#> [1] NA

# Example usage with typical dplyr pipeline
data.frame(
  x = c(1, 2, 3, NA, NA, NA),
  y = c("a", "a", "a", "b", "b", "b")
) |>
  dplyr::group_by(y) |>
  dplyr::summarise(
    base_max_x = base::max(x, na.rm = TRUE), # for comparison
    base_min_x = base::min(x, na.rm = TRUE), # for comparison
    max_x = max(x, na.rm = TRUE),
    min_x = min(x, na.rm = TRUE)
  )
#> Warning: There were 2 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `base_max_x = base::max(x, na.rm = TRUE)`.
#> ℹ In group 2: `y = "b"`.
#> Caused by warning in `base::max()`:
#> ! no non-missing arguments to max; returning -Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
#> # A tibble: 2 × 5
#>   y     base_max_x base_min_x max_x min_x
#>   <chr>      <dbl>      <dbl> <dbl> <dbl>
#> 1 a              3          1     3     1
#> 2 b           -Inf        Inf    NA    NA
```
