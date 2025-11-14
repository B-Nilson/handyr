# Delete specific entries from a database table.

Delete specific entries from a database table.

## Usage

``` r
delete_database_entries(db, table_name, entry_keys)
```

## Arguments

- db:

  The database connection or path to the database file (if sqlite or
  duckdb file extension).

- table_name:

  The name of the table to delete entries from.

- entry_keys:

  A data frame or named vector with names matching those in the table
  and entries equal to the those in the rows to delete.
