read_from_database <- function(db, table_name, query_fun = \(df) df, collect = TRUE) {
  # Handle db path instead of connection
  # TODO: wont work for postgres
  if (is.character(db)) {
    type <- tools::file_ext(db)
    db <- .dbi_drivers[[type]][[1]]() |>
      DBI::dbConnect(db)
  }
  
  # Connect to table and build query
  query <- db |> 
    dplyr::tbl(table_name) |> 
    query_fun()
  
  # Either collect results or return lazy table of query
  if (collect) {
    output <- dplyr::collect(query)
  }else {
    output <- query
  }
  return(output)
}