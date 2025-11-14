# Convert between common units

`convert_units` is a wrapper around
[`units::set_units()`](https://r-quantities.github.io/units/reference/units.html)
to switch between common measurement units.

## Usage

``` r
convert_units(x, from, to, keep_units = FALSE, ...)
```

## Arguments

- x:

  A numeric vector.

- from, to:

  A string indicating the current/desired unit of `x`. Run
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  for a list of valid units for your system.

- keep_units:

  A logical value indicating whether to keep the units after conversion
  (default is `FALSE`).

- ...:

  Additional arguments passed on to
  [`units::set_units()`](https://r-quantities.github.io/units/reference/units.html)

## Value

A numeric vector with the same length/type as `x`. If
`keep_units = TRUE`, the units attribute is preserved

## Examples

``` r
convert_units(c(1, 2, 3), from = "cm", to = "km")
#> [1] 1e-05 2e-05 3e-05
convert_units(c(1, 2, 3), from = "degF", to = "degC")
#> [1] -17.22222 -16.66667 -16.11111
convert_units(c(1, 2, 3), from = "ft", to = "m")
#> [1] 0.3048 0.6096 0.9144
convert_units(c(1, 2, 3), from = "oz", to = "L")
#> [1] 0.02957353 0.05914706 0.08872059
convert_units(c(1, 2, 3), from = "m/s", to = "km/h")
#> [1]  3.6  7.2 10.8
convert_units(c(1, 2, 3), from = "ug/m3", to = "mg/km3")
#> [1] 1e+06 2e+06 3e+06
```
