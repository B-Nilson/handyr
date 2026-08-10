#' Replace out-of-range values with the nearest in-range value
#'
#' `clamp` sets values less than `min(range)` to `min(range)` and values greater than `max(range)` to `max(range)`
#'
#' @param x A numeric or date/POSIXct vector.
#' @param range A numeric or date/POSIXct vector of length two to clamp `x` to.
#'   Must be the same type as `x`.
#'   * If `range[1]` is `NA` (default), then `min(x)` will be used.
#'   * If `range[2]` is `NA` (default), then `max(x)` will be used.
#'
#' @return
#' A numeric or date/POSIXct vector equal to `x` with:
#'  * values less than `min(range)` set to `min(range)`
#'  * values greater than `max(range)` set to `max(range)`
#'
#' @export
#'
#' @examples
#' clamp(1:5, range = c(2, NA)) # 2, 2, 3, 4, 5
#' clamp(1:5, range = c(2, 4)) # 2, 2, 3, 4, 4
#' clamp(1:5, range = c(-1, 1)) # 1, 1, 1, 1, 1
clamp <- function(x, range = c(NA, NA)) {
  # Handle inputs
  stopifnot(
    is.numeric(x) || lubridate::is.Date(x) || lubridate::is.POSIXct(x)
  )
  stopifnot(length(range) == 2)
  if (!all(is.na(range))) {
    stopifnot(
      "`range` must be numeric, Date, POSIXct, or NA values." = is.numeric(
        range
      ) ||
        lubridate::is.Date(range) ||
        lubridate::is.POSIXct(range)
    )
    x_type <- if (is.numeric(x)) {
      "numeric"
    } else if (lubridate::is.Date(x)) {
      "Date"
    } else {
      "POSIXct"
    }
    range_type <- if (is.numeric(range)) {
      "numeric"
    } else if (lubridate::is.Date(range)) {
      "Date"
    } else {
      "POSIXct"
    }
    if (!identical(x_type, range_type)) {
      stop(
        "`x` is ",
        x_type,
        " but `range` is ",
        range_type,
        "; both must be the same type (numeric, Date, or POSIXct)."
      )
    }
  }

  # set NA range values to min/max
  if (is.na(range[1])) {
    range[1] <- min(x, na.rm = TRUE)
  }
  if (is.na(range[2])) {
    range[2] <- max(x, na.rm = TRUE)
  }
  # ensure range is sorted
  range <- sort(range)

  # clamp values
  x |>
    pmax(range[1]) |>
    pmin(range[2])
}
