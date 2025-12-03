# Create table if not already existing

Create a table in a database if it doesn't already exist. This is
alternative to
[`DBI::dbCreateTable()`](https://dbi.r-dbi.org/reference/dbCreateTable.html)
with additional features for creating primary keys, unique indexes, and
indexes.

## Usage

``` r
create_database_table(
  db,
  table_name,
  new_data,
  primary_keys = NULL,
  unique_indexes = NULL,
  indexes = NULL,
  partition_by = NULL,
  partition_type = c("range", "list", "hash")[1],
  insert_data = TRUE
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
  key on the combination of `col1` and `col2`).

- unique_indexes:

  A list of character vector(s) of column names to use as unique
  indexes. These will be added to the table, in addition to the primary
  key, and will result in an error if non-unique data is
  inserted/existing. Indexes speed up queries by allowing for faster
  lookups, but can increase the size of the database and reduce write
  performance.

- indexes:

  A list of character vector(s) of column names to use as indexes.

- partition_by:

  (UNDER DEVELOPMENT) A named list of partitions to create, where names
  correspond to the names of the columns to partition by, and values
  correspond to the values of that column within each partition (i.e.
  `partition_by = list(gear = list(c(1, 4), c(4, 6)), carb = list(c(1, 4), c(4, 10)))`
  will create partition the table into 2 partitions, one for gears 1,2,
  and 3, and one for gear 4 and 5). True partitioning is only supported
  for Postgres, but for other types mulitiple partition tables will be
  created and linked using a View. If `NULL` (the default), no
  partitioning will be used.

- partition_type:

  (UNDER DEVELOPMENT) A character string specifying the type of
  partitioning to use. Currently supports "range" (expects pairs of
  inclusive start and exclusive end values), "list" (expects vectors of
  values to match), and "hash" (expects single values). Default is
  "range". Ignored if `partition_by` is `NULL`.

- insert_data:

  A logical indicating whether to insert the data frame provided in
  `new_data` into the created table. If `FALSE`, the table will be
  created but no data will be inserted.

## Value

A data frame containing the number of rows inserted into the table.
