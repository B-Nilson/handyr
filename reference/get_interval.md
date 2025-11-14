# Determine distance(s) between values

`get_interval` finds the most common interval in a vector. If there are
multiple intervals of the same frequency, it takes the first one in the
sorted vector and a warning is raised.

## Usage

``` r
get_interval(x, most_common = TRUE, na.rm = FALSE, quiet = FALSE)
```

## Arguments

- x:

  A numeric, Date, POSIXt, or POSIXct vector.

- most_common:

  A logical value indicating whether the most common interval (default)
  should be returned or all intervals and their frequencies in a sorted
  data frame.

- na.rm:

  A logical value indicating whether NAs should be removed before
  calculating the interval frequencies. Default is `FALSE`.

- quiet:

  A logical value indicating whether warnings should be suppressed and
  output should be invisible. Default is `FALSE`.

## Value

- If `most_common = TRUE` (default), a single numeric or difftime value.

- If `most_common = FALSE`, a sorted data frame with columns `interval`
  (numeric or difftime) and `frequency` (integer).

## Examples

``` r
get_interval(c(1, 3, 5:10))
#> [1] 1
get_interval(as.Date(c("2020-01-01", "2020-01-03", "2020-01-05", "2020-01-06")))
#> Time difference of 2 days
get_interval(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:02", "2020-01-01 00:00:04")))
#> Time difference of 2 secs
get_interval(c(0, 3, 5, 7:10), most_common = FALSE)
#>   interval frequency
#> 1        1         3
#> 2        2         2
#> 3        3         1
```
