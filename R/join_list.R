#' Join a list of data frames
#'
#' `join_list` takes a list of data frames and joins them together using a specified join type.
#'
#' @param df_list A list of data frames.
#' @param by A character vector of column names to join on.
#' @param mode A character string specifying the type of join to perform. Must be one of "full", "inner", "left", "right", ... (see [dplyr::join]).
#'
#' @return A single data frame containing the joined data.
#'
#' @export
join_list <- function(df_list, by, mode = "full") {
  join_fun_name <- paste0("dplyr::", mode, "_join")
  join_fun <- get(join_fun_name)
  df_list |> Reduce(f = \(...) join_fun(..., by = by))
}

