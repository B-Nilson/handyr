#' Control what happens when an error/warning occurs
#'
#' `on_error`/`on_warning` provide a simple way to handle errors/warnings by specifying a value to be returned instead as well as if a message or warning/error should be displayed instead.
#' 
#' @param ... Something to run and capture errors/warnings (if any).
#' @param .return What is to be returned if an error/warning occurs.
#'   Default is `NULL`
#' @param .message A logical value indicating if the error message should be displayed as a message instead.
#'   Default is `FALSE`
#' @param .warn,.stop A logical value indicating if the error/warning message should be displayed as a warning/error instead.
#'   Default is `FALSE`
#'
#' @return the output of `...` unless an error/warning occurs, then `invisible(.return)` instead.
#' @export
#'
#' @examples
#' on_error(stop("test"), .return = -1, .message = TRUE)
#' on_error(read.csv("not_A_fil3.123"), .return = NULL)
#' on_warning(warning("test"), .return = -1, .message = TRUE)
#' on_warning(base::max(NA, na.rm = TRUE), .return = NULL)
on_error <- function(..., .return = NULL, .message = FALSE, .warn = FALSE) {
  # Handle inputs
  stopifnot(is.logical(.message), length(.message) == 1)
  stopifnot(is.logical(.warn), length(.warn) == 1)

  # Run input, catch errors if any and control response
  tryCatch(..., error = \(e){
    if (.message) message(as.character(e))
    if (.warn) warning(as.character(e))
    return(invisible(.return))
  })
}

#' @rdname on_error
#' @export
on_warning <- function(..., .return = NULL, .message = FALSE, .stop = FALSE) {
  # Handle inputs
  stopifnot(is.logical(.message), length(.message) == 1)
  stopifnot(is.logical(.stop), length(.stop) == 1)

  # Run input, catch warnings if any and control response
  tryCatch(..., warning = \(e){
    if (.message) message(as.character(e))
    if (.stop) stop(as.character(e))
    return(invisible(.return))
  })
}
