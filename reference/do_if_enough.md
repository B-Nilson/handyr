# Apply a function if enough values are non-NA

`do_if_enough`:

- checks if enough non-NA values are in `x`. (see `.min_non_na`)

- if so, applies a function (`FUN`) to `x`.

- if not, returns `.return`. (see
  [`do_if()`](https://b-nilson.github.io/handyr/reference/do_if.md) for
  more details)

## Usage

``` r
do_if_enough(x, FUN, ..., .min_non_na = 1, .return = NA)
```

## Arguments

- x:

  A vector (numeric, character, etc).

- FUN:

  A function to be applied to `x`.

- ...:

  Additional arguments to be passed to `FUN`.

- .min_non_na:

  The minimum length of `x` (ignoring NAs) to be considered enough.

- .return:

  What to return instead of the output of `FUN` if not enough non-NA
  values.

## Value

The output of FUN applied to x if enough values are provided, `.return`
otherwise

## Examples

``` r
do_if_enough(c(1, 2, 3), mean, .min_non_na = 2)
#> [1] 2
do_if_enough(c(1, 2, NA), mean, .min_non_na = 2)
#> [1] 1.5
do_if_enough(c(1, NA, NA), mean, .min_non_na = 2)
#> [1] NA
do_if_enough(c(1, NA, NA), mean, .min_non_na = 2, .return = -1)
#> [1] -1
```
