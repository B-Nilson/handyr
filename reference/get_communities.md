# Get a communities within a region from OpenStreetMap

Get a communities within a region from OpenStreetMap

## Usage

``` r
get_communities(
  bbox,
  types = c("city", "town", "village", "hamlet"),
  timeout = 6000,
  quiet = TRUE
)
```

## Arguments

- bbox:

  Usually the name of an area (ie. "Fort St. John, BC"). See
  [`osmdata::getbb()`](https://docs.ropensci.org/osmdata/reference/getbb.html)
  for more information.

- types:

  A character vector of types of communities to search for. Default is
  c("city", "town", "village", "hamlet"). See
  <https://wiki.openstreetmap.org/wiki/Key:place>.

- timeout:

  The time in seconds to wait for a response from the OSM API. Default
  is 6000 seconds (10 minutes).

- quiet:

  A logical value indicating whether non-critical warnings/messages
  should be suppressed. Default is TRUE.

## Value

An `sf` data frame containing the communities in the bounding box with
columns "osm_id", "type" (community type), "name",
"population"/"population_source" (if available), and "geometry".

## Examples

``` r
get_communities("Fort St. John, BC")
#> Simple feature collection with 1 feature and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -120.8469 ymin: 56.2524 xmax: -120.8469 ymax: 56.2524
#> Geodetic CRS:  WGS 84
#>                osm_id type          name population                  geometry
#> 2865722923 2865722923 city Fort St. John      18609 POINT (-120.8469 56.2524)

get_communities("Yukon, Canada")
#> Simple feature collection with 65 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -140.8773 ymin: 60.00194 xmax: -128.7068 ymax: 68.98877
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                osm_id    type               name population
#> 664245432   664245432  hamlet          Keno City         24
#> 2737836444 2737836444 village   Stewart Crossing         25
#> 9181339576 9181339576  hamlet   Two Mile Village         79
#> 2138092203 2138092203    town           Carcross        317
#> 2705565763 2705565763    town         Ross River        382
#> 2668706359 2668706359    town               Faro        400
#> 2700798050 2700798050    town           Carmacks        503
#> 2976400400 2976400400    city         Whitehorse      27889
#> 107350205   107350205  hamlet          Ruby Camp         NA
#> 107350219   107350219  hamlet Mendenhall Landing         NA
#>                              geometry
#> 664245432  POINT (-135.3022 63.90958)
#> 2737836444  POINT (-136.679 63.37544)
#> 9181339576 POINT (-128.7439 60.08715)
#> 2138092203 POINT (-134.7087 60.16679)
#> 2705565763  POINT (-132.4495 61.9798)
#> 2668706359 POINT (-133.3553 62.22954)
#> 2700798050 POINT (-136.2899 62.08877)
#> 2976400400 POINT (-135.0549 60.72157)
#> 107350205  POINT (-137.9167 61.08333)
#> 107350219  POINT (-136.0167 60.76667)
```
