db_create_table <- function(
    db,
    tbl_name,
    new_data = data.frame(id = 1),
    unique_indexes = list("id"),
    overwrite = FALSE,
    temporary = FALSE
) {
    if (is.character(db)) {  
        type <- tools::file_ext(db)
        db <- .dbi_drivers[[type]][[1]]() |>
            DBI::dbConnect(db)
    }

    # Initialize table
    db |>
        DBI::dbCreateTable(
            name = tbl_name,
            overwrite = overwrite,
            temporary = temporary,
            fields = new_data
        )

    # Add unique restraint(s) # TODO: fails for sqlite....
    sql_queries <- unique_indexes |>
        sapply(\(unique_ids) {
            constraint_name <- paste0(
                "unique_",
                unique_ids |> paste0(collapse = "_")
            )
            "ALTER TABLE %s ADD CONSTRAINT %s UNIQUE (%s);" |>
                sprintf(
                    tbl_name,
                    constraint_name,
                    unique_ids |> paste0(collapse = ", ")
                )
        })
    for (sql_query in sql_queries) {
        db |> DBI::dbExecute(sql_query)
    }

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
}
