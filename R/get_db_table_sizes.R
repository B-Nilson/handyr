db_table_sizes <- function(db) {
  query <- db |>
    get_db_type() |>
    get_table_size_query()
  db |> DBI::dbGetQuery(query)
}

get_db_type <- function(db) {
  db_pkg <- class(db) |> attr("package")
  if (!db_pkg %in% sapply(.dbi_drivers, names)) {
    stop("Unknown database type for package: ", db_pkg)
  }
  names(.dbi_drivers)[which(sapply(.dbi_drivers, names) == db_pkg)]
}


get_table_size_query <- function(db_type = "postgresql") {
  stopifnot(
    "Only defined for postgresql and duckdb backends" = db_type %in%
      c("postgresql", "duckdb")
  )
  switch(
    db_type,
    "postgresql" = "
      SELECT 
        schemaname,
        relname AS table_name,
        pg_size_pretty(pg_total_relation_size(relid)) AS total_size,
        pg_size_pretty(pg_relation_size(relid)) AS table_size,
        pg_size_pretty(pg_indexes_size(relid)) AS index_size,
        pg_size_pretty(pg_total_relation_size(relid) - pg_relation_size(relid) - pg_indexes_size(relid)) AS toast_size
      FROM pg_catalog.pg_statio_user_tables
      ORDER BY pg_total_relation_size(relid) DESC;",
    "duckdb" = "
      SELECT 
        table_name,
        estimated_size
      FROM duckdb_tables
      ORDER BY estimated_size DESC;"
  )
}
