get_season <- function(dates = Sys.time(), use_autumn = FALSE) {
  stopifnot(is.POSIXct(dates) | is.Date(dates))
  stopifnot(length(dates) >= 1)
  stopifnot(is.logical(use_autumn), length(use_autumn) == 1)
  # TODO: default use_autumn based on system local?
  # Need at least 2 for arr.ind to work right
  if (length(dates) == 1) {
    drop_last_date <- TRUE
    dates <- c(dates, Sys.time())
  } else {
    drop_last_date <- FALSE
  }
  months_in_seasons <- list(
    Spring = c(3, 4, 5),
    Summer = c(6, 7, 8),
    Fall = c(9, 10, 11),
    Winter = c(12, 1, 2)
  )

  if (use_autumn) {
    names(months_in_seasons)[3] <- "Autumn"
  }

  which_season <- months_in_seasons |>
    sapply(\(months) lubridate::month(dates) %in% months) |>
    which(arr.ind = TRUE)

  seasons <- names(months_in_seasons)[which_season[, 2]]
  if (drop_last_date) {
    seasons <- seasons[-length(seasons)]
  }
  return(seasons)
}
