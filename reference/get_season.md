# Get the season from a date

`get_season` takes a date (or set of dates) and returns the
corresponding season(s).

## Usage

``` r
get_season(
  dates = Sys.time(),
  as_factor = FALSE,
  include_year = FALSE,
  include_months = FALSE,
  use_autumn = FALSE
)
```

## Arguments

- dates:

  A Date or POSIXct vector of dates to get the season for. Defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

- as_factor:

  A logical indicating whether the output should be a factor or a
  character vector. Defaults to `FALSE`.

- include_year:

  A logical indicating whether the output should include the year of the
  season or not. Defaults to `FALSE`.

- include_months:

  A logical indicating whether the output should include the months of
  the season or not. Defaults to `FALSE`.

- use_autumn:

  A logical indicating whether "Autumn" should be used instead of
  "Fall". Defaults to `FALSE`.

## Value

If `as_factor = FALSE` (default), a character vector with the season(s)
and optionally the year/months. If `as_factor = TRUE`, a factor of the
above with chronological levels.
