#' Summarise logs stored from repeated `log` calls
#'
#' @param logs A list of `log` values
#'
#' @description
#' `summarise_logs` takes a list of `log` values and returns a summary of the time taken between each log call.
#'
#' @family Utilities
#'
#' @return a character vector indicating the summary of the time taken between each log call
#' @export
#'
#' @examples
#' logs <- list(
#'   log("test", quiet = TRUE),
#'   log("test", quiet = TRUE),
#'   log("test", quiet = TRUE),
#'   log("test", quiet = TRUE),
#'   log("test", quiet = TRUE),
#'   log("test", quiet = TRUE)
#' )
#' summarise_logs(logs)
summarise_logs <- function(logs) {
  summary <- logs |>
    for_each(as.data.frame, .bind = TRUE) |>
    dplyr::mutate(
      delta = timestamp |> difftime(dplyr::lag(timestamp)),
      text = text |>
        paste0(": ", dplyr::lead(delta), " ", attr(delta, "units"))
    )

  total_time <- difftime(logs[[length(logs)]]$timestamp, logs[[1]]$timestamp)
  time_units <- attr(total_time, "units")

  message(
    paste(
      "Total time:", total_time, time_units, "\n-->",
      summary$text[-length(summary$text)] |>
        paste(collapse = "\n--> ")
    )
  )
}
