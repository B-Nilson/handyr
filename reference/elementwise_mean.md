# Vectorized element-wise mean, optionally ignoring NA values

Vectorized element-wise mean, optionally ignoring NA values

## Usage

``` r
elementwise_mean(..., digits = NULL, na.rm = FALSE)
```

## Arguments

- ...:

  Vectors to compute the element-wise mean of. Must all be the same
  length.

- digits:

  Number of digits to round to (optional, default is `NULL` for no
  rounding).

- na.rm:

  If TRUE, NA values are ignored.

## Value

Vector of the element-wise mean of `...` with the same length as vectors
provided.
