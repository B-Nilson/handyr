# Read data from a database

`read_from_database` takes a database connection and table name, applies
a dplyr-based query, and returns either a lazy-table or a data frame of
the query results.

## Usage

``` r
read_from_database(
  db,
  table_name,
  query_fun = NULL,
  collect = TRUE,
  pull = NULL
)
```

## Arguments

- db:

  A database connection or the path to a database file (if sqlite or
  duckdb file extension).

- table_name:

  A character string specifying the table to read from.

- query_fun:

  A function taking a single argument (the data.frame-like object to be
  queried) and returning a data frame. Default is `NULL`, which does not
  apply a query fun. Most `dplyr` functions will be implemented here
  (see
  [dbplyr::dbplyr](https://dbplyr.tidyverse.org/reference/dbplyr-package.html)
  for details). (e.g.
  `query_fun = \(df) df |> dplyr::select(column1, column2)`) Anything
  else beyond variable names needs to be prefaced with `!!` (e.g.
  `... |> dplyr::filter(month |> dplyr::between(!!select_months[1], !!select_months[2]))`).

- collect:

  A logical value indicating whether to use
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
  to fetch the data from the database. Will be overidden if `pull` is
  not `NULL`. Default is `TRUE`.

- pull:

  A single character string to use with
  [`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html) to
  pull a column after applying `query_fun`. Default is `NULL`, which
  does not pull any columns.

## Value

`query_fun` applied to the table `table_name` in `db` where if:

- `collect` is `TRUE` (the default), returns a tibble (data.frame)

- `collect` is `FALSE`, returns a lazy table of the query

- **Unless** `pull` is a column name, returns a vector of the specified
  column
