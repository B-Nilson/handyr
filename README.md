
# handyr <a href="https://b-nilson.github.io/handyr/"><img src="man/figures/logo.svg" align="right" height="139" alt="handyr website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/B-Nilson/handyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/B-Nilson/handyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/B-Nilson/handyr/graph/badge.svg)](https://app.codecov.io/gh/B-Nilson/handyr)
<!-- badges: end -->

The goal of handyr is to streamline common datascience tasks with wrappers around various handy packages in a consistent style.

## Installation

You can install the development version of handyr from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("B-Nilson/handyr")
```

## Examples

### Create and write/read/delete from Databases

``` r
library(handyr)

# Create a file-based database (or setup postgresql server if on Windows)
db <- create_database("test", type = "sqlite")
# or type = "duckdb"
# or type = "postgresql" (if on Windows)

# Write data to an existing database
db |> 
  write_to_database(
    new_data = datasets::airquality,
    table_name = "airquality",
    primary_keys = c("Month", "Day"),
    insert_new = TRUE, # set to FALSE to ignore entries not already in db
    update_duplicates = FALSE # set to TRUE to update existing entries in db where overlap exists
  )

# Read data from a database, leverage dplyr for common sql queries
db |>
  read_from_database(
    table_name = "airquality",
    query_fun = \(df) df |> dplyr::filter(Month == 5, Day == 5),
    # Set to FALSE to return a lazy tbl of the query instead of loading full results into memory
    collect = TRUE 
  )

# Delete matching entries in a database table
db |>
  delete_database_entries(
    table_name = "airquality",
    entry_keys = data.frame(Month = 5, Day = 5:8)
  )

```

### Record/log Steps in a Workflow

``` r
library(handyr)

# Initiate logging entry
logs = list(log_step("handyr Examples", header = TRUE, time = FALSE))

# Then log each step
logs$something <- log_step("doing something")

# do something 

# End logging
logs$complete <- log_step("done")

# Summarise logs timing
summarise_logs(logs)

```

### Forget about for loops

``` r

# TODO: examples showcasing for_each

```

### Spatial / Temporal Data Manipulation

``` r

# Local timezone lookup
get_timezone(lng = -105.053144, lat = 69.116178)

# Detailed season description
c(Sys.time() - 10^7, Sys.time()) |>
  get_season(as_factor = TRUE, include_year = TRUE, include_months = TRUE)

# Split large date ranges into specific-size chunks
c(Sys.time() - 10^8, Sys.time()) |>
  split_date_range(max_duration = "120 days")

# Get most likely data interval (useful for when occasional gaps may exist)
get_interval(c(1:10, 12, 14, 16:20)

# Convert sf objects back to data frames cleanly
cities <- data.frame(
    name = c("Nanaimo", "Port Moody", "Prince George"),
    x = c(-124.0531, -122.8519, -122.7949),
    y = c(49.1633, 49.2844, 53.8934)
  )
cities_sf <- cities |>
  sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
sf_as_df(cities_sf)

# Or just get the x/y coordinates as a data.frame
extract_sf_coords(cities_sf)

```

### Miscellaneous Quality of Life Improvments

``` r
library(handyr)

# Vector operations
x <- c(1:10 + 0.123, NA) |>
  clamp(range = c(2, 9)) |> # replace values outside range with nearest value
  swap(2, with = NA) |> # swap out all "2"s with NA
  rolling("mean", width = 3, direction = "backwards", .min_non_na = 2) |> # 3-point (fast) rolling mean
  truncate(digits = 1) |> # drop all digits after the first
  convert_units(from = "m", to = "km") |> # convert from metres to kilometres
  max(na.rm = TRUE) # take the max (or `min()` or `mode()`) with consistent NA handling

# Error handling
load_your_data <- function(x) {
  if(x %in% c(2, 4)) {
    stop("Something went wrong") # simulate file not existing or some other error
  } else{
    data.frame(x = x, y = x^2) # simulate data being loaded in
  } 
}
your_data <- 1:5 |> for_each(
  # Capture error, convert to warning, return NULL, keep going
  # or use a custom warning message instead (i.e. `.warn = "Failed to load data."`)
  \(x) load_your_data(x) |> on_error(.return = NULL, .warn = TRUE)
  .bind = TRUE
)

# TODO: showcase join_list, do_if, save_figure, silence
# TODO: showcase max/min NA handling improvements
# TODO: showcase rolling built-ins speed improvements

```

