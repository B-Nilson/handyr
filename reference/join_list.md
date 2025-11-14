# Join a list of data frames

`join_list` takes a list of data frames and joins them together using a
specified join type.

## Usage

``` r
join_list(df_list, by = NULL, mode = "full")
```

## Arguments

- df_list:

  A list of data frames.

- by:

  A character vector of column names to join on. Default (NULL) joins on
  all matching columns.

- mode:

  A character string specifying the type of join to perform. Must be one
  of "full", "inner", "left", "right", ... (see
  [dplyr::join](https://dplyr.tidyverse.org/reference/mutate-joins.html)).

## Value

A single data frame containing the joined data.
