write_to_database <- function(
    db,
    tbl_name,
    new_data,
    primary_key,
    unique_indexes = NULL,
    update_duplicates = FALSE
) {
    # Try removing staging table in case it already exists
    tbl_name_staged <- paste0("_", tbl_name, "_staged")
    db |>
        DBI::dbRemoveTable(tbl_name_staged) |>
        on_error(.return = NULL)

    # Create the staged table
    staged <- db |>
        db_create_table(
            new_data = new_data,
            tbl_name = tbl_name_staged,
            primary_key = primary_key,
            unique_indexes = unique_indexes,
            overwrite = TRUE
        ) |>
        on_error(.return = NULL, .warn = TRUE) # TODO: handle differently?

    # Merged overlaps/new data as needed from staged to exisiting table
    # TODO: primary key?
    result <- db |>
        db_transaction({
            # Update values already in database
            if (update_duplicates) {
                db |>
                    db_merge_overlap(
                        new_data = new_data,
                        tbl_name_a = tbl_name,
                        tbl_name_b = tbl_name_staged,
                        primary_key = primary_key
                    )
            }

            # Insert values not already there
            db |>
                db_insert_new(
                    new_data = new_data,
                    tbl_name_a = tbl_name,
                    tbl_name_b = tbl_name_staged,
                    primary_key = primary_key
                )
        })

    # Remove "_staged" table
    db |> DBI::dbRemoveTable(tbl_name_staged)
}

db_create_table <- function(
    db,
    tbl_name,
    new_data,
    primary_key,
    unique_indexes = NULL,
    overwrite = FALSE,
    temporary = FALSE
) {
    if (is.character(db)) {
        type <- tools::file_ext(db)
        db <- .dbi_drivers[[type]][[1]]() |>
            DBI::dbConnect(db)
    }

    # Define SQL templates
    create_template <- "CREATE TABLE %s (\n%s,\n%s,\n%s\n);"
    primary_key_template <- "\tPRIMARY KEY (%s)"
    column_def_template <- '\t"%s" %s'
    unique_constraint_template <- "\tUNIQUE (%s)"

    # Build primary key SQL
    # TODO: quotes dont work for MySQL (backticks) or SQL server (sqr brackets)
    primary_key_sql <- paste0('"', primary_key, '"') |>
        paste0(collapse = ", ")
    primary_key_sql <- primary_key_template |>
        sprintf(primary_key_sql)

    # Build column definition SQL
    column_types <- new_data |>
        get_sql_column_types(unique_indexes = unique_indexes)
    column_def_sql <- column_def_template |>
        sprintf(
            names(new_data),
            column_types
        ) |>
        paste(collapse = ",\n")

    # Build unique constraint SQL
    if (is.null(unique_indexes)) {
        unique_constraint_sql <- ""
    } else {
        unique_indexes <- unique_indexes |>
            sapply(\(unique_ids) {
                unique_ids <- paste0('"', unique_ids, '"') |>
                    paste0(collapse = ", ")
                unique_constraint_template |>
                    sprintf(unique_ids)
            })
        unique_constraint_sql <- unique_constraints |>
            paste(collapse = ",\n")
    }

    # Build table creation query
    create_query <- create_template |>
        sprintf(
            tbl_name,
            column_def_sql,
            primary_key_sql,
            unique_constraint_sql
        )

    # Create table
    db |> DBI::dbExecute(create_query)

    # insert rows if provided
    if (nrow(new_data)) {
        db |>
            dplyr::copy_to(
                df = new_data,
                name = tbl_name,
                overwrite = overwrite,
                temporary = temporary
            )
    }
    invisible(dplyr::tbl(db, tbl_name))
}

get_sql_column_types <- function(new_data, unique_indexes = NULL) {
    column_types <- new_data |>
        sapply(class)
    sql_types <- dplyr::case_when(
        column_types %in%
            c("integer", "logical", "Date", "POSIXct", "POSIXlt", "POSIXt") ~
            "INTEGER",
        column_types == "numeric" ~ "REAL",
        TRUE ~ "TEXT"
    )
    if (!is.null(unique_indexes)) {
        is_unique <- names(new_data) %in% unlist(unique_indexes)
        sql_types[is_unique] <- paste(sql_types[is_unique], "NOT NULL")
    }
    return(sql_types)
}

db_merge_overlap <- function(
    db,
    tbl_name_a,
    tbl_name_b,
    primary_key
) {
    # Handle db path instead of connection
    if (is.character(db)) {
        type <- tools::file_ext(db)
        db <- .dbi_drivers[[type]][[1]]() |>
            DBI::dbConnect(db)
    }

    # Builder header renaming sql
    match_header_template <- '\t"%s" = s."%s"'
    col_names <- head(dplyr::tbl(db, tbl_name_a), 1) |>
        dplyr::collect() |>
        colnames()
    match_header_sql <- new_header_template |>
        sprintf(
            col_names[col_names != primary_key],
            col_names[col_names != primary_key]
        ) |>
        paste(collapse = ",\n")

    # Build overlap test sql
    overlap_test_template <- '%s."%s" = s."%s"'
    overlap_test_sql <- overlap_test_template |>
        sprintf(
            rep(tbl_name_a, length(primary_key)),
            primary_key,
            primary_key
        ) |>
        paste(collapse = " AND ")

    merge_template <- "UPDATE %s\nSET\n%s\nFROM %s s\nWHERE %s;"
    merge_query <- merge_template |>
        sprintf(
            tbl_name_a,
            match_header_sql,
            tbl_name_b,
            overlap_test_sql
        )

    db |> DBI::dbExecute(merge_query)
}

db_insert_new <- function(
    db,
    tbl_name_a,
    tbl_name_b,
    primary_key
) {
    # Build header insert sql
    col_names <- dplyr::tbl(db, tbl_name_a) |> 
        head(1) |>
        dplyr::collect() |>
        colnames()
    col_names <- col_names[col_names != primary_key]
    header_insert_sql <- paste0('"', col_names, '"') |> 
        paste(collapse = ", ")

    # Build header select sql
    header_select_sql <- paste0('s."', col_names, '"') |>
        paste(collapse = ", ")

    # Build overlap test sql
    overlap_test_template <- 's."%s" = o."%s"'
    overlap_test_sql <- overlap_test_template |>
        sprintf(primary_key, primary_key) |>
        paste(collapse = " AND ")

    # Build not overlap test sql
    not_overlap_test_template <- 'o."%s" IS NULL'
    not_overlap_test_sql <- not_overlap_test_template |>
        sprintf(primary_key) |>
        paste(collapse = " AND ")

    # Build insert query
    insert_template <- "INSERT INTO %s (%s)\nSELECT %s\nFROM %s s LEFT JOIN %s o ON %s\nWHERE %s;"
    sql_query <- insert_template |> 
        sprintf(
            tbl_name_a,
            header_insert_sql,
            header_select_sql,
            tbl_name_b,
            tbl_name_a,
            overlap_test_sql,
            not_overlap_test_sql
        )
    db |> DBI::dbExecute(sql_query)
}

db_transaction <- function(db, ...) {
    # Begin a transaction
    DBI::dbBegin(db)
    # run code block passed to function, capture error as warning
    result <- on_error(
        .return = NULL,
        .warn = TRUE,
        ...
    )
    # Rollback the transaction if failed
    if (is.null(result)) {
        DBI::dbRollback(db)
    } else {
        # Commit the transaction, capture error if needed
        result2 <- DBI::dbCommit(db) |>
            on_error(.return = NULL, .warn = TRUE)
        # Rollback the transaction if failed
        if (is.null(result2)) {
            DBI::dbRollback(db)
        }
    }

    return(invisible())
}
