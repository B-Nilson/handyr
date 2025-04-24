# Calculates the mean if enough values are provided
# TODO: document, test, and export
mean_if_enough <- function(x, min_n = 0, ...) {
  ifelse(sum(!is.na(x)) >= min_n, mean(x, na.rm = T, ...), NA)
}

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
      FUN = mean_if_enough, min_n = min_n
    ) |>
    round(digits = digits)
}