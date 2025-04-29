#' Easy warning handler
#'
#' @param ... A code block (typically wrapped in `{}`) to run and capture warnings (if any).
#' @param .return (Optional) What is to be returned if a warning occurs (instead of throwing a warning).
#'   Default is `NULL`
#' @param .message (Optional) A single logical (TRUE/FALSE) value indicating if the error message should be displayed as a message instead.
#'   Default is `FALSE`
#' @param .stop (Optional) A single logical (TRUE/FALSE) value indicating if the warning message should be displayed as an error instead.
#'   Default is `FALSE`
#'
#' @description
#' `on_warning` provides a simple way to handle warnings by specifying a value to be returned on warning and/or if a message/error should be displayed instead.
#'
#' @family Utilities
#'
#' @return the output of `...` unless a warning occurs, then `return` instead.
#' @export
#'
#' @examples
#' on_warning(warning("test"), .return = -1, .message = TRUE)
#' on_warning(base::max(NA, na.rm = TRUE), .return = NULL)
on_warning <- function(..., .return = NULL, .message = FALSE, .stop = FALSE) {
  tryCatch(..., warning = \(e){
    if (.message) message(as.character(e))
    if (.stop) stop(as.character(e))
    return(.return)
  })
}
