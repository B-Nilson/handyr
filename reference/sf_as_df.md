# Convert an sf object to a data.frame

This function removes the geometry from an sf object, effectively
converting it back to a data.frame.

## Usage

``` r
sf_as_df(sf_obj, keep_coords = FALSE)
```

## Arguments

- sf_obj:

  An object of class `sf`, typically created by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)

- keep_coords:

  A logical value indicating if the coordinates should be kept when
  dropping the geometry

## Value

A `data.frame` object without the geometry column.

## Examples

``` r
cities <- data.frame(
  name = c("Nanaimo", "Port Moody", "Prince George"),
  x = c(-124.0531, -122.8519, -122.7949),
  y = c(49.1633, 49.2844, 53.8934)
)
cities_sf <- cities |>
  sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
sf_as_df(cities_sf)
#>            name
#> 1       Nanaimo
#> 2    Port Moody
#> 3 Prince George
sf_as_df(cities_sf, keep_coords = TRUE)
#>            name         x       y   crs
#> 1       Nanaimo -124.0531 49.1633 WGS84
#> 2    Port Moody -122.8519 49.2844 WGS84
#> 3 Prince George -122.7949 53.8934 WGS84
```
