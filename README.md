
# handyr

<!-- badges: start -->
<!-- badges: end -->

The goal of handyr is to streamline common datascience tasks with wrappers around various handy packages in a consistent style.

## Installation

You can install the development version of handyr from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("B-Nilson/handyr")
```

## Examples

``` r
library(handyr)

# Initiate logging entry
logs = list(log_step("handyr Examples", header = TRUE, time = FALSE))
# Then log each step
logs$summaries <- log_step("Vector Functions")
x <- c(1:10 + 0.123, NA) |>
  clamp(range = c(2, 9)) |> # replcae values outside range with nearest value
  swap(2, with = NA) |> # swap out all "2"s with NA
  rolling(mean, width = 3, direction = "backwards", .min_non_na = 2) |> # 3-point rolling mean
  truncate(digits = 1) |> # drop all digits after the first
  convert_units(from = "m", to = "km") |> # convert from metres to kilometres
  max(na.rm = TRUE) # take the max (or `min()` or `mode()`)

logs$others <- log_step("Location Functions")
tz <- get_timezone(lng = -105.053144, lat = 69.116178)

cities <- data.frame(
    name = c("Nanaimo", "Port Moody", "Prince George"),
    x = c(-124.0531, -122.8519, -122.7949),
    y = c(49.1633, 49.2844, 53.8934)
  )
cities_sf <- cities |>
  sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
sf_as_df(cities_sf)
extract_sf_coords(cities_sf)

logs$others <- log_step("QOL Functions")
load_your_data <- function(x) {
  if(x %in% c(2, 4)) {
    stop("Something went wrong") # simulate file not existing or some other error
  } else{
    data.frame(x = x, y = x^2) # simulate data being loaded in
  } 
}
your_data <- 1:5 |> for_each(
  \(x) load_your_data(x) |> on_error(.return = NULL), 
  .bind = TRUE
)
get_interval(your_data$x)

# End logging and summarise
logs$complete = log_step("Completed Successfully")
summarise_logs(logs)

```

