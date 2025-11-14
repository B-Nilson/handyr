# Swap out values in a vector

`swap` provides a simple way to switch out certain values in a vector.
It is useful for replacing NA's, Infinites, and erroneous values.

## Usage

``` r
swap(x, what, with)
```

## Arguments

- x:

  Vector of values to be have certain values swapped out.

- what:

  One or more values to be replaced with `with` throughout `x`.

- with:

  A single value to replace `what` for throughout `x`.

## Value

a vector of `x` where all instances of `what` are replaced with `with`

## See also

Other Utilities:
[`summarise_logs()`](https://b-nilson.github.io/handyr/reference/summarise_logs.md)

## Examples

``` r
swap(c(-20:20, NA), what = NA, with = -1)
#>  [1] -20 -19 -18 -17 -16 -15 -14 -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2
#> [20]  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
#> [39]  18  19  20  -1
swap(c(-20:20, Inf), what = Inf, with = NA)
#>  [1] -20 -19 -18 -17 -16 -15 -14 -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2
#> [20]  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
#> [39]  18  19  20  NA
swap(c(-20:20), what = Inf, with = NA)
#>  [1] -20 -19 -18 -17 -16 -15 -14 -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2
#> [20]  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
#> [39]  18  19  20
```
