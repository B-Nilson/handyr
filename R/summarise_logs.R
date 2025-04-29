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
#' @importFrom rlang .data
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
      # TODO: clean this up (skip making delta, just do lead difftime)
      delta = .data$timestamp |> difftime(dplyr::lag(.data$timestamp)),
      runtime = dplyr::lead(.data$delta),
      units = attr(.data$delta, "units"),
      text = .data$text |>
        paste0(": ", .data$runtime, " ", .data$units)
    )

  total_time <- logs[[length(logs)]]$timestamp |>
    difftime(logs[[1]]$timestamp)
  time_units <- total_time |> attr("units")

  message(
    paste(
      "Total time:", total_time, time_units,
      "\n-->", summary$text[-length(summary$text)] |>
        paste(collapse = "\n--> ")
    )
  )
}
