# TODO: add tests
#' Easy error handler
#'
#' @param ... A code block (typically wrapped in `{}`) to run and capture errors (if any).
#' @param return (Optional) What is to be returned if an error occurs (instead of throwing an error).
#'   Default is `NULL`
#' @param msg (Optional) A single logical (TRUE/FALSE) value indicating if the error message should be displayed as a message instead.
#'   Default is `FALSE`
#' @param warn (Optional) A single logical (TRUE/FALSE) value indicating if the error message should be displayed as a warning instead.
#'   Default is `FALSE`
#'
#' @description
#' `on_error` provides a simple way to handle errors by specifying a value to be returned on error and/or if a message/warning should be displayed instead.
#'  This is especially useful when attempting to load in multiple data files where it is possible for files not to exist.
#'
#' @family Utilities
#'
#' @return the output of `...` unless an error occurs, then `return` instead.
#' @export
#'
#' @examples
#' on_error(stop("test"), return = NULL, msg = TRUE)
#' on_error(read.csv("not_A_fil3.123"), return = NULL)
on_error <- function(..., return = NULL, msg = FALSE, warn = FALSE) {
  tryCatch(..., error = \(e){
    if (msg) message(as.character(e))
    if (warn) warning(as.character(e))
    return(return)
  })
}
