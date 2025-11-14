# Create a file-based database

This function creates a new database using either sqlite or duckdb. See
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) for
more details.

## Usage

``` r
create_database(
  name,
  type = NULL,
  path = NULL,
  version = NULL,
  credentials = c("USER", "PASSWORD"),
  port = 5432,
  return_connection = TRUE
)
```

## Arguments

- name:

  The name of (and optionally path to) the database to create. If the
  file extension does not match the type, it will be added.

- type:

  The type of database to create. If NULL (the default), the type will
  be derived from `name`, defaulting to "sqlite".

- path:

  The location to save the database file to. If NULL (the default), the
  path will be derived from `name`, defaulting to the current working
  directory.

- version:

  The version of the database to create. Currently only used for
  `type = "postgresql"`. Default is `NULL` which uses `"17.0-1"`.

- credentials:

  A vector of length 2 containing the username and password for the
  database. Currently only used for `type = "postgresql"`. Default is
  `c("USER", "PASSWORD")`.

- port:

  The port to use for the database server. Currently only used for
  `type = "postgresql"`. Default is `5432`.

- return_connection:

  A logical value indicating whether to return the database connection
  instead of the path to the database file. Default is `TRUE`.
