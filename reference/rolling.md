# Apply a function over a rolling window

`rolling` applies a funtion (`FUN`) over a moving window (see `.width`
and `.direction`) for each element of `x` using
[`zoo::rollapply()`](https://rdrr.io/pkg/zoo/man/rollapply.html).

## Usage

``` r
rolling(
  x,
  FUN = mean,
  ...,
  .width = 3,
  .direction = "backward",
  .fill = NA,
  .min_non_na = 0
)
```

## Arguments

- x:

  A vector.

- FUN:

  A function to be applied to each window of `x`. If is a character
  string, it will be converted to a function using
  [`get()`](https://rdrr.io/r/base/get.html), unless it is in "sum",
  "mean", "min", or "max" which have built-in vectorized (faster)
  implementations. Default is `"mean"`.

- ...:

  Additional arguments to be passed to `FUN`.

- .width:

  A numeric value indicating the width of the window. See
  [`zoo::rollapply()`](https://rdrr.io/pkg/zoo/man/rollapply.html) for
  more details. Default is `3`.

- .direction:

  A character string or numeric value indicating the direction of the
  window. Options are `"backward"` (1), `"forward"` (-1), or `"center"`
  (0). See the `align` argument in
  [`zoo::rollapply()`](https://rdrr.io/pkg/zoo/man/rollapply.html) for
  more details. Default is `"backward"`.

- .fill:

  (Optional) A single value to be used for filling in any values that
  are outside of the window. See
  [`zoo::rollapply()`](https://rdrr.io/pkg/zoo/man/rollapply.html) for
  more details. Default is `NA`.

- .min_non_na:

  (Optional) A single numeric value indicating the minimum number of
  observations required to compute a value. Default is `0`.

## Value

A vector of the same length as `x` and type of the output of `FUN`.

## Details

- Be sure to fill any gaps in `x` and ensure `x` is sorted, otherwise
  the window may be applied incorrectly.

- Values outside of the window are replaced with `.fill`.

- `.min_non_na` is passed to
  [`do_if_enough()`](https://b-nilson.github.io/handyr/reference/do_if_enough.md)
  to only apply `FUN` if enough values are present in the window.

## Examples

``` r
x <- c(1, 2, 3, 4, 5)
x |> rolling(mean, .width = 2)
#> [1]  NA 1.5 2.5 3.5 4.5
x |> rolling(mean, .width = 2, .direction = "forward", .fill = -1)
#> [1]  1.5  2.5  3.5  4.5 -1.0
```
