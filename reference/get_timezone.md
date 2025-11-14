# Lookup timezones of latitude/longitude pairs

`get_timezone` uses the `lutz` package to lookup the timezone of
locations from lat/lng coords. See
[lutz::tz_lookup_coords](http://andyteucher.ca/lutz/reference/tz_lookup_coords.md)
for more details.

## Usage

``` r
get_timezone(lng, lat, method = "accurate", quiet = FALSE, ...)
```

## Arguments

- lng, lat:

  Numeric longitude/latitude pairs (decimal degrees).

- method:

  either "accurate" or "fast", indicating the lookup method to use. See
  [lutz::tz_lookup_coords](http://andyteucher.ca/lutz/reference/tz_lookup_coords.md)
  for more details. Set `.quiet` to `TRUE` to suppress warning produced
  when `method = "fast"`.

- quiet:

  A logical value indicating if messages and warnings should be
  suppressed.

- ...:

  Additional parameters to pass to
  [lutz::tz_lookup_coords](http://andyteucher.ca/lutz/reference/tz_lookup_coords.md)
  (currently unused)

## Value

a character vector with the same length as `lat` and `lng` indicating
each locations likely timezone. See
[lutz::tz_lookup_coords](http://andyteucher.ca/lutz/reference/tz_lookup_coords.md)
for more details.

## Examples

``` r
get_timezone(lng = -105.053144, lat = 69.116178, method = "accurate")
#> [1] "America/Cambridge_Bay"
get_timezone(lng = c(-105.053144, -106.053144), lat = c(69.116178, 49.116178), method = "fast")
#> Warning: Using 'fast' method. This can cause inaccuracies in time zones
#>   near boundaries away from populated ares. Use the 'accurate'
#>   method if accuracy is more important than speed.
#> [1] "America/Cambridge_Bay" "America/Regina"       
```
