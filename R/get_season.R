#' Get the season from a date
#'
#' `get_season` takes a date (or set of dates) and returns the corresponding season(s).
#'
#' @param dates A Date or POSIXct vector of dates to get the season for.
#'   Defaults to `Sys.time()`.
#' @param as_factor A logical indicating whether the output should be a factor or a character vector.
#'   Defaults to `FALSE`.
#' @param include_year A logical indicating whether the output should include the year of the season or not.
#'   Defaults to `FALSE`.
#' @param include_months A logical indicating whether the output should include the months of the season or not.
#'   Defaults to `FALSE`.
#' @param use_autumn A logical indicating whether "Autumn" should be used instead of "Fall".
#'   Defaults to `FALSE`.
#'
#' @return
#'   If `as_factor = FALSE` (default), a character vector with the season(s) and optionally the year/months.
#'   If `as_factor = TRUE`, a factor of the above with chronological levels.
#' @export
get_season <- function(
    dates = Sys.time(),
    as_factor = FALSE,
    include_year = FALSE,
    include_months = FALSE,
    use_autumn = FALSE) {
  stopifnot(lubridate::is.POSIXct(dates) | lubridate::is.Date(dates))
  stopifnot(length(dates) >= 1)
  stopifnot(is.logical(as_factor), length(as_factor) == 1)
  stopifnot(is.logical(include_year), length(include_year) == 1)
  stopifnot(is.logical(include_months), length(include_months) == 1)
  stopifnot(is.logical(use_autumn), length(use_autumn) == 1)
  # TODO: default use_autumn based on system local?

  # Define which months are in each season
  months_in_seasons <- list(
    Spring = 3:5,
    Summer = 6:8,
    Fall = 9:11,
    Winter = c(12, 1, 2)
  )

  # Need at least 2 dates for arr.ind to work right
  last_date_is_placeholder <- length(dates) == 1
  if (last_date_is_placeholder) {
    dates <- c(dates, Sys.time())
  }

  # Use Autumn instead of Fall if requested
  if (use_autumn) {
    names(months_in_seasons)[3] <- "Autumn"
  }

  # Determine which season each date is in
  which_season <- months_in_seasons |>
    sapply(\(months) lubridate::month(dates) %in% months) |>
    which(arr.ind = TRUE)
  seasons <- names(months_in_seasons)[which_season[order(which_season[, 1]), 2]]

  # Remove placeholder date if inserted earlier
  if (last_date_is_placeholder) {
    dates <- dates[-length(dates)]
    seasons <- seasons[-length(seasons)]
  }
  output <- seasons # store for modifying as needed without side effects

  # Include year if requested
  if (include_year) {
    year_mod <- ifelse(seasons == "Winter" & lubridate::month(dates) != 12, 1, 0)
    season_years <- lubridate::year(dates) - year_mod
    output <- output |> paste(season_years)
  }

  # Include months if requested
  if (include_months) {
    month_letters <- month.name |>
      stringr::str_sub(end = 1)
    season_month_letters <- months_in_seasons |>
      sapply(\(months) month_letters[months] |> paste(collapse = ""))
    season_months <- season_month_letters[match(seasons, names(season_month_letters))]
    output <- output |> paste0(" [", season_months, "]")
  }

  # Convert to factor if requested
  if (as_factor) {
    # Get factor levels in correct order
    out_levels <- data.frame(
      season = seasons |>
        factor(levels = names(months_in_seasons)),
      year = if (include_year) factor(season_years) else NA,
      months = if (include_months) paste0("[", season_months, "]") else NA
    ) |>
      dplyr::distinct() |>
      dplyr::arrange("year", "season") |>
      dplyr::select(dplyr::where(~ !all(is.na(.x)))) |>
      tidyr::unite("levels", dplyr::everything(), sep = " ") |>
      dplyr::pull(levels)
    # Convert to factor
    output <- output |>
      factor(levels = out_levels)
  }

  return(output)
}
