# Write data to a database

Write data to a database, with options to insert new data, update
existing data, and create a new table.

## Usage

``` r
write_to_database(
  db,
  table_name,
  new_data,
  primary_keys,
  unique_indexes = NULL,
  indexes = NULL,
  insert_new = TRUE,
  update_duplicates = FALSE,
  use_on_conflict = FALSE,
  skip_checks = FALSE
)
```

## Arguments

- db:

  A database connection or path to database (if sqlite or duckdb file
  extension).

- table_name:

  A character string specifying the table to write to.

- new_data:

  A data frame to write to the table.

- primary_keys:

  A character vector of column names to use as the primary key, the main
  identifier of individual rows in a table. Multiple columns can be
  specified and uniqueness will be assessed based on the combination of
  columns. (e.g. `primary_keys = c("col1", "col2")` will add a primary
  key on the combination of `col1` and `col2`). Primary keys are
  functionally similiar to "unique indexes", but act as the main row
  identifier.

- unique_indexes:

  A list of character vector(s) of column names to use as unique
  indexes. These will be added to the table, in addition to the primary
  key, and will result in an error if non-unique data is
  inserted/existing. Indexes speed up queries by allowing for faster
  lookups, but can increase the size of the database and reduce write
  performance. Multiple columns can be specified and uniqueness will be
  assessed based on the combination of columns. (e.g.
  `unique_indexes = list(c("col1", "col2"))` will add a unique index on
  the combination of `col1` and `col2`.). If `NULL` (the default), no
  unique indexes will be added.

- indexes:

  A list of character vector(s) of column names to use as indexes. These
  will be added to the table, in addition to the primary key and unique
  indexes. Indexes speed up queries by allowing for faster lookups, but
  can increase the size of the database and reduce write performance.
  Multiple columns can be specified for composite indexes, and the index
  will be named after the list names if provided. If `NULL` (the
  default), no indexes will be added.

- insert_new:

  A logical value indicating if new data should be inserted into the
  existing table. If `FALSE`, no new data will be inserted, only
  existing rows will be updated if `update_duplicates = TRUE`. Ignored
  if `skip_checks` is `TRUE`. Default is `TRUE`.

- update_duplicates:

  A logical value indicating if existing rows should be updated with new
  data. If `FALSE`, no existing rows will be updated. Only new data will
  be inserted if `insert_new = TRUE`. Ignored if `skip_checks` is
  `TRUE`. Default is `FALSE`.

- use_on_conflict:

  A logical value indicating if the `ON CONFLICT` clause should be used
  when updating existing rows. This will be faster for bulk inserts, but
  requires a unique contraint on the provided `primary_keys`, and `db`
  must support `ON CONFLICT` (e.g. SQLite, Postgres). If `FALSE`, the
  `ON CONFLICT` clause will not be used.

- skip_checks:

  A logical value indicating if checks for overlapping data should be
  skipped. If `TRUE`,
  [`DBI::dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
  will be used to insert new data, and will fail if there are
  overlapping primary keys. Default is `FALSE`.

## Value

An invisible db connection.

## Details

It will create the table and insert `new_data` if the table does not
already exist. Otherwise:

- If `insert_new = TRUE`, `new_data` will be inserted into the existing
  table.

- If `update_duplicates = TRUE`, existing data will be updated with the
  overlap of `new_data`.
