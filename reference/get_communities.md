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
#> Error in httr2::req_perform(req): HTTP 504 Gateway Timeout.

get_communities("Yukon, Canada")
#> Error in httr2::req_perform(req): HTTP 504 Gateway Timeout.
```
