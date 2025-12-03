# Create a connection to a database

Currently supports sqlite, duckdb, and PostgreSQL databases. Must
provide connection details for PostgreSQL databases (see
[`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)).

Be sure to call
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
on the database connection when finished working with it.

## Usage

``` r
connect_to_database(name, type = NULL, path = NULL, ...)
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

- ...:

  Additional arguments passed to
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  "example.sqlite" |> create_database(type = "sqlite")
  db <- connect_to_database(name = "example.sqlite")
  DBI::dbDisconnect(db)
} # }
```
