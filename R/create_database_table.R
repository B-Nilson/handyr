#' Create table if not already existing
#'
#' Create a table in a database if it doesn't already exist.
#' This is alternative to [DBI::dbCreateTable()] with additional features for creating primary keys, unique indexes, and indexes.
#'
#' @param db A database connection or path to database (if sqlite or duckdb file extension).
#' @param table_name A character string specifying the table to write to.
#' @param new_data A data frame to write to the table.
#' @param primary_keys A character vector of column names to use as the primary key, the main identifier of individual rows in a table.
#'   Multiple columns can be specified and uniqueness will be assessed based on the combination of columns.
#'   (e.g. `primary_keys = c("col1", "col2")` will add a primary key on the combination of `col1` and `col2`).
#' @param unique_indexes A list of character vector(s) of column names to use as unique indexes.
#'   These will be added to the table, in addition to the primary key, and will result in an error if non-unique data is inserted/existing.
#'   Indexes speed up queries by allowing for faster lookups, but can increase the size of the database and reduce write performance.
#' @param indexes A list of character vector(s) of column names to use as indexes.
#' @param partition_by A named list of partitions to create, where names correspond to the names of the columns to partition by, and values correspond to the values of that column within each partition (i.e. `partition_by = list(gear = list(c(1, 4), c(4, 6)), carb = list(c(1, 4), c(4, 10)))` will create partition the table into 2 partitions, one for gears 1,2, and 3, and one for gear 4 and 5).
#'   True partitioning is only supported for Postgres, but for other types mulitiple partition tables will be created and linked using a View.
#'   If `NULL` (the default), no partitioning will be used.
#' @param partition_type A character string specifying the type of partitioning to use.
#'   Currently supports "range" (expects pairs of inclusive start and exclusive end values), "list" (expects vectors of values to match), and "hash" (expects single values).
#'   Default is "range".
#'   Ignored if `partition_by` is `NULL`.
#' @param insert_data A logical indicating whether to insert the data frame provided in `new_data` into the created table.
#'   If `FALSE`, the table will be created but no data will be inserted.
#' @return A data frame containing the number of rows inserted into the table.
#' @export
create_database_table <- function(
  db,
  table_name,
  new_data,
  primary_keys = NULL,
  unique_indexes = NULL,
  indexes = NULL,
  partition_by = NULL,
  partition_type = c("range", "list", "hash")[1],
  insert_data = TRUE
) {
  if (is.character(db)) {
    db <- connect_to_database(db)
  }
  is_postgres <- inherits(db, "PqConnection") | inherits(db, "PostgreSQL")
  is_partitioned <- !is.null(partition_by)
  table_name_safe <- table_name |>
    DBI::dbQuoteIdentifier(conn = db)

  # Partition input data if applicable
  if (is_partitioned) {
    partitioned_data <- new_data |>
      partition_data(
        partition_by = partition_by,
        partition_type = partition_type
      )
  }

  # Create partition tables and a master view if applicable
  if (is_partitioned & !is_postgres) {
    # make partition tables # TODO: abstract
    partition_names <- partitioned_data |>
      dplyr::group_by(.data$.partition) |>
      dplyr::mutate(
        partition_name = table_name |>
          paste0("_", paste(unlist(dplyr::cur_group()), collapse = "_"))
      ) |>
      apply(1, \(row) {
        row <- as.list(row)
        db |>
          create_database_table(
            table_name = row$partition_name,
            new_data = row$data,
            primary_keys = primary_keys,
            unique_indexes = unique_indexes,
            indexes = indexes,
            insert_data = insert_data
          )
        return(row$partition_name)
      })
    # create master view
    unions <- "SELECT * FROM %s" |>
      sprintf(partition_names) |>
      paste(collapse = " UNION ALL ")
    create_view_query <- "CREATE VIEW %s AS %s;" |>
      sprintf(table_name_safe, unions)
    db |>
      DBI::dbExecute(create_view_query)
    return(invisible()) # return early
  }

  # Build column definition SQL
  column_types <- db |>
    get_sql_column_types(new_data = new_data, unique_indexes = unique_indexes)
  column_def_sql <- '\t"%s" %s' |>
    sprintf(names(new_data), column_types) |>
    paste(collapse = ",\n")

  # Build primary key SQL
  if (is.null(primary_keys)) {
    primary_key_sql <- ""
  } else {
    primary_keys_safe <- primary_keys |> DBI::dbQuoteIdentifier(conn = db)
    primary_key_sql <- "\tPRIMARY KEY (%s)" |>
      sprintf(paste0(primary_keys_safe, collapse = ", "))
  }

  # Build unique constraint SQL
  if (is.null(unique_indexes)) {
    unique_constraint_sql <- ""
  } else {
    unique_constraint_sql <- unique_indexes |>
      sapply(\(unique_ids) {
        unique_ids_safe <- unique_ids |>
          DBI::dbQuoteIdentifier(conn = db) |>
          paste0(collapse = ", ")
        "\tUNIQUE (%s)" |> sprintf(unique_ids_safe)
      }) |>
      paste(collapse = ",\n")
  }

  # Build table creation query - defer constraints if postgresql for speed
  create_query <- "CREATE TABLE %s (\n%s%s%s\n);" |>
    sprintf(
      table_name_safe,
      column_def_sql,
      (is.null(primary_keys) | is_postgres) |>
        ifelse("", paste(",\n", primary_key_sql)),
      (is.null(unique_indexes) | is_postgres) |>
        ifelse("", paste(",\n", unique_constraint_sql))
    )

  # Mark table as partitioned if postgres and partition_by is provided
  if (is_partitioned & is_postgres) {
    partition_cols <- names(partition_by) |>
      DBI::dbQuoteIdentifier(conn = db) |>
      paste0(collapse = ", ")
    partition_sql <- " PARTITION BY %s (%s);" |>
      sprintf(toupper(partition_type), partition_cols)
    create_query <- create_query |>
      sub(pattern = ";", replacement = partition_sql)
  }

  # Create table
  db |> DBI::dbExecute(create_query)

  # Create partition tables if postgres and partition_by is provided
  if (is_partitioned & is_postgres) {
    partition_names <- partitioned_data |>
      dplyr::group_by(.data$.partition) |>
      dplyr::mutate(
        partition_name = table_name |>
          paste0("_", .data$.partition) |>
          DBI::dbQuoteIdentifier(conn = db),
        .min_sql = paste0(
          dplyr::across(dplyr::starts_with(".range_"), \(x) unlist(x)[1]),
          collapse = ", "
        ),
        .max_sql = paste0(
          dplyr::across(dplyr::starts_with(".range_"), \(x) unlist(x)[2]),
          collapse = ", "
        ),
        # TODO: figure out how to handle list partitions
        partition_def = #ifelse(
          # partition_type == "RANGE",
          # yes = 
          "FROM (%s) TO (%s)" |>
            sprintf(.min_sql, .max_sql),
          # no = "IN (%s)" |>
          #   sprintf(
          #     as.character(.vals) |>
          #       DBI::dbQuoteLiteral(conn = db) |>
          #       paste(collapse = ", ")
          #   )
        # )
      ) |>
      apply(1, \(row) {
        row <- as.list(row)
        create_partition_query <- "CREATE TABLE %s PARTITION OF %s FOR VALUES %s;" |>
          sprintf(row$partition_name, table_name_safe, row$partition_def)
        db |>
          DBI::dbExecute(create_partition_query)
        return(row$partition_name)
      })
  }

  # insert rows if provided
  if (nrow(new_data) & insert_data) {
    success <- db |>
      DBI::dbWriteTable(
        value = new_data,
        name = table_name,
        append = TRUE
      )
    n_rows_inserted <- ifelse(success, nrow(new_data), 0)
  } else {
    n_rows_inserted <- 0
  }

  # Add constraints if defered
  if (is_postgres) {
    pkey_query <- "ALTER TABLE %s ADD %s;" |>
      sprintf(table_name_safe, primary_key_sql)
    db |>
      DBI::dbExecute(pkey_query)
    if (!is.null(unique_indexes)) {
      # TODO: test this
      constraint_query <- "ALTER TABLE %s ADD CONSTRAINT %s;" |>
        sprintf(table_name_safe, unique_constraint_sql)
      db |>
        DBI::dbExecute(constraint_query)
    }
  }

  # Add indexes if provided
  if (length(indexes) > 0) {
    for (i in 1:length(indexes)) {
      index_name <- NULL
      if (!(is.null(names(indexes)) | names(indexes)[i] == "")) {
        index_name <- names(indexes)[i]
      }
      db |>
        create_database_index(
          table_name = table_name,
          index_cols = indexes[[i]],
          index_name = index_name
        )
    }
  }
  invisible(n_rows_inserted)
}

create_database_index <- function(
  db,
  table_name,
  index_cols,
  index_name = NULL
) {
  table_name_safe <- table_name |>
    DBI::dbQuoteIdentifier(conn = db)
  safe_cols <- index_cols |>
    DBI::dbQuoteIdentifier(conn = db)
  if (is.null(index_name)) {
    index_name <- index_cols |>
      gsub(pattern = " |\\.", replacement = "_") |>
      paste(collapse = "_")
  }

  index_query <- "CREATE INDEX IF NOT EXISTS %s_%s ON %s (%s);" |>
    sprintf(
      table_name |> gsub(pattern = " |\\.", replacement = "_"),
      index_name,
      table_name_safe,
      safe_cols |> paste(collapse = ", ")
    )
  db |>
    DBI::dbExecute(index_query)
}

partition_data <- function(new_data, partition_by, partition_type) {
  partitioned_data <- new_data
  for (col in names(partition_by)) {
    for (i in seq_along(partition_by[[col]])) {
      if (partition_type == "range") {
        within <- partition_by[[col]][[i]]
        partitioned_data <- partitioned_data |>
          dplyr::mutate(
            .partition = .data[[col]] >= within[1] & .data[[col]] < within[2],
            .range = list(within)
          ) |>
          dplyr::rename_with(.cols = c(.partition, .range), .fn = \(x) {
            paste0(x, "_", col, "_", paste(within, collapse = "to"))
          })
      } else if (partition_type == "list") {} else {}
    }
    partitioned_data <- partitioned_data |>
      dplyr::group_by(dplyr::across(
        dplyr::starts_with(paste0(".partition_", col))
      )) |>
      dplyr::mutate(
        .partition = names(dplyr::cur_group())[unlist(dplyr::cur_group())] |>
          sub(pattern = ".partition_", replacement = "") |>
          paste(collapse = "_"),
        .range = data.frame(
          row = dplyr::cur_group_rows(),
          col = match(
            paste0(".range_", .partition),
            colnames(partitioned_data)
          )
        ) |>
          as.matrix() |>
          apply(
            1,
            \(entry) {
              entry <- as.list(entry)
              partitioned_data[entry$row, entry$col] |>
                unlist() |>
                unname() |>
                as.list()
            }
          )
      ) |>
      dplyr::ungroup() |>
      dplyr::rename_with(.cols = c(.partition, .range), .fn = \(x) {
        paste0(x, "_", col)
      }) |>
      dplyr::select(
        -dplyr::starts_with(paste0(c(".partition_", ".range_"), col, "_"))
      )
  }
  partitioned_data |>
    dplyr::group_by(dplyr::across(
      dplyr::starts_with(paste0(".partition_"))
    )) |>
    dplyr::mutate(
      .partition = unlist(dplyr::cur_group()) |>
        paste(collapse = "_")
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::starts_with(".partition_")) |>
    dplyr::group_nest(.partition, dplyr::across(dplyr::starts_with(".range_")))
}
