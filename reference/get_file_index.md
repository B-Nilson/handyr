# Get a file index from a URL

This function fetches a file index from a URL for a remote directory.
The file index is a data frame with columns for the file/directory name,
last modified date, size, and type (file or dir).

## Usage

``` r
get_file_index(url, size_unit = "kB", timeout = 2)
```

## Arguments

- url:

  The URL to fetch the file index from

- size_unit:

  The unit to express the file sizes in. Defaults to "kB".

- timeout:

  The timeout in seconds for the httr request. Defaults to 2.

## Examples

``` r
get_file_index("https://aqmap.ca/aqmap/outputs/")
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `size = convert_units(...)`.
#> Caused by warning in `shorten_number()`:
#> ! NAs introduced by coercion
#> # A tibble: 3 × 4
#>   name                                 last_modified       size type 
#>   <chr>                                <dttm>              [kB] <fct>
#> 1 aqmap_sensor_selection_polygons.html 2025-11-18 14:00:00 5100 file 
#> 2 aqsu_past_2_week.rds                 2025-12-03 05:00:00 3000 file 
#> 3 screenshots/                         2025-11-18 14:26:00   NA dir  
```
