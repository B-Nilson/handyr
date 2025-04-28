#' Perform a rolling calculation on a vector if enough values are provided.
#'
#' @param x A vector of values
#' @param FUN (Optional) A function to be applied to each window of `x`.
#'   The default is `mean`.
#' @param ... (Optional) Additional arguments to be passed to `FUN`
#' @param .width (Optional) A single numeric value indicating the width of the window.
#'   The default is `3`.
#' @param .direction (Optional) A single character string or numeric value indicating the direction of the window.
#'   The default is `"backward"`.
#'   Options are `"backward"` (1), `"forward"` (-1), or `"center"` (0).
#' @param .fill (Optional) A single value to be used for filling in any values that are outside of the window.
#'   The default is `NA`.
#' @param .min_length (Optional) A single numeric value indicating the minimum number of observations required to
#'   compute a value.
#'   The default is `0`.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' x |> rolling(mean, width = 2)
#' rolling(x, width = 2, direction = "forward", fill = -1)
#' @export
# TODO: code without zoo (use dplyr::lag/lead)
rolling <- function(x, FUN = mean, ..., .width = 3, .direction = "backward", .fill = NA, .min_length = 0) {
  rlang::check_installed("zoo")
  align <- ifelse(
    .direction %in% c("backward", 1), "right",
    ifelse(.direction %in% c("forward", -1), "left",
      ifelse(.direction %in% c("center", 0), "center", NA)
    )
  )
  x |>
    zoo::rollapply(
      width = .width, align = align, fill = .fill,
      FUN = \(x) do_if_enough(x, FUN, .min_length = .min_length)
    )
}
