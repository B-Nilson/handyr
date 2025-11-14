# Add sf coordinates and crs as columns

`extract_sf_coords` adds the x and y coordinates and the crs as
additional columns to an `sf` object, with the option to convert it to a
`data.frame`.

## Usage

``` r
extract_sf_coords(sf_obj, keep_sf = TRUE, add_crs = TRUE)
```

## Arguments

- sf_obj:

  An object of class `sf`, typically created by
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)

- keep_sf:

  A logical value indicating if the return object should remain an `sf`
  object or be converted to a `data.frame`

- add_crs:

  A logical value indicating if the crs should be added as a column

## Value

If `keep_sf` is:

- `TRUE` `sf_obj` with two additional numeric columns (`x` and `y`) and
  a character column (`crs`).

- `FALSE`: same as above, but converted to a `data.frame`

## Examples

``` r
cities <- data.frame(
  name = c("Nanaimo", "Port Moody", "Prince George"),
  x = c(-124.0531, -122.8519, -122.7949),
  y = c(49.1633, 49.2844, 53.8934)
)
cities_sf <- cities |>
  sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
cities_sf |>
  extract_sf_coords()
#> Simple feature collection with 3 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -124.0531 ymin: 49.1633 xmax: -122.7949 ymax: 53.8934
#> Geodetic CRS:  WGS 84
#>            name                  geometry         x       y   crs
#> 1       Nanaimo POINT (-124.0531 49.1633) -124.0531 49.1633 WGS84
#> 2    Port Moody POINT (-122.8519 49.2844) -122.8519 49.2844 WGS84
#> 3 Prince George POINT (-122.7949 53.8934) -122.7949 53.8934 WGS84
```
