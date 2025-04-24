# Calculates rolling mean if enough non-na provided
# TODO: code without zoo (use dplyr::lag/lead)
# TODO: document, test, and export
roll_mean <- function(x, width, direction = "backward", fill = NA, min_n = 0, digits = 0) {
  align <- ifelse(direction == "backward", "right",
    ifelse(direction == "forward", "left", "center")
  )
  x |>
    zoo::rollapply(
      width = width, align = align, fill = fill,
      FUN = \(x) do_if_enough(x, mean, .min_length = min_n)
    ) |>
    round(digits = digits)
}