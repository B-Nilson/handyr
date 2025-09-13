#' Apply a function over a rolling window
#'
#' `rolling` applies a funtion (`FUN`) over a moving window (see `.width` and `.direction`) for each element of `x` using [zoo::rollapply()].
#'
#' * Be sure to fill any gaps in `x` and ensure `x` is sorted, otherwise the window may be applied incorrectly.
#' * Values outside of the window are replaced with `.fill`.
#' * `.min_non_na` is passed to [do_if_enough()] to only apply `FUN` if enough values are present in the window.
#'
#' @param x A vector.
#' @param FUN A function to be applied to each window of `x`.
#'   If is a character string, it will be converted to a function using [get()],
#'   unless it is in "sum", "mean", "min", or "max" which have built-in vectorized (faster) implementations.
#'   Default is `"mean"`.
#' @param ... Additional arguments to be passed to `FUN`.
#' @param .width A numeric value indicating the width of the window.
#'   See [zoo::rollapply()] for more details.
#'   Default is `3`.
#' @param .direction A character string or numeric value indicating the direction of the window.
#'   Options are `"backward"` (1), `"forward"` (-1), or `"center"` (0).
#'   See the `align` argument in [zoo::rollapply()] for more details.
#'   Default is `"backward"`.
#' @param .fill (Optional) A single value to be used for filling in any values that are outside of the window.
#'   See [zoo::rollapply()] for more details.
#'   Default is `NA`.
#' @param .min_non_na (Optional) A single numeric value indicating the minimum number of observations required to
#'   compute a value.
#'   Default is `0`.
#'
#' @return A vector of the same length as `x` and type of the output of `FUN`.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' x |> rolling(mean, .width = 2)
#' x |> rolling(mean, .width = 2, .direction = "forward", .fill = -1)
#' @export
rolling <- function(
  x,
  FUN = "mean",
  ...,
  .width = 3,
  .direction = "backward",
  .fill = NA,
  .min_non_na = 0
) {
  rlang::check_installed("zoo")
  # Handle inputs
  stopifnot(is.function(FUN) | is.character(FUN))
  stopifnot(is.numeric(.width), length(.width) == 1)
  stopifnot(
    is.numeric(.direction) | is.character(.direction),
    length(.direction) == 1
  )
  stopifnot(is.null(.fill) | length(.fill) == 1)
  stopifnot(is.numeric(.min_non_na), length(.min_non_na) == 1)

  # Handle FUN
  if (is.character(FUN)) {
    # Use built in vectorized (faster) implementations where available
    built_ins <- c("sum", "mean", "max", "min")
    if (FUN %in% built_ins & .direction != "center") {
      roll_fun <- paste0("roll_", FUN) |> get()
      rolling_val <- x |>
        roll_fun(
          width = .width,
          direction = .direction,
          fill = .fill,
          min_non_na = .min_non_na
        )
      return(rolling_val)
    }
    FUN <- get(FUN)
  }

  # translate .direction -> align for zoo
  align <- ifelse(
    .direction %in% c("backward", 1),
    "right",
    ifelse(
      .direction %in% c("forward", -1),
      "left",
      ifelse(.direction %in% c("center", 0), "center", NA)
    )
  )

  # Apply rolling function with specified window/fill
  # using `do_if_enough` to only apply function if enough values
  x |>
    zoo::rollapply(
      width = .width,
      align = align,
      fill = .fill,
      FUN = \(x) do_if_enough(x, FUN, .min_non_na = .min_non_na)
    )
}

# Vectorized rolling mean
roll_mean <- function(
  x,
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0
) {
  rolling_sum <- x |>
    roll_sum(
      width = width,
      direction = direction,
      fill = fill,
      min_non_na = min_non_na,
      .include_counts = TRUE
    )
  n_non_missing <- swap(attr(rolling_sum, "n_non_missing"), what = 0, with = NA)
  as.numeric(rolling_sum) / n_non_missing
}

# Vectorized rolling sum
roll_sum <- function(
  x,
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0,
  .include_counts = FALSE
) {
  value_matrix <- x |>
    build_roll_matrix(
      width = width,
      direction = direction,
      fill = fill
    )
  n_missing <- rowSums(is.na(value_matrix))
  rolling_sum <- rowSums(value_matrix, na.rm = TRUE)
  rolling_sum[n_missing < min_non_na] <- NA

  if (.include_counts) {
    n_possible <- rep(width, length(x))
    n_possible[1:width] <- 1:width
    attr(rolling_sum, "n") <- n_possible
    attr(rolling_sum, "n_non_missing") <- width - n_missing
  }

  if (!is.null(fill)) {
    rolling_sum[1:(width - 1)] <- fill
  }

  return(rolling_sum)
}

roll_max <- function(
  x,
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0
) {
  roll_maxmin(x, type = "max", width, direction, fill, min_non_na)
}

roll_min <- function(
  x,
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0
) {
  roll_maxmin(x, type = "min", width, direction, fill, min_non_na)
}

roll_maxmin <- function(
  x,
  type = "max",
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0
) {
  value_matrix <- x |>
    build_roll_matrix(
      width = width,
      direction = direction,
      fill = fill
    )
  FUN <- ifelse(type == "max", pmax, pmin)
  rolling_maxmin <- do.call(
    \(...) FUN(..., na.rm = TRUE),
    as.data.frame(value_matrix)
  )

  n_missing <- rowSums(is.na(value_matrix))
  rolling_maxmin[n_missing < min_non_na] <- NA

  if (!is.null(fill)) {
    rolling_maxmin[1:(width - 1)] <- fill
  }
  return(rolling_maxmin)
}

build_roll_matrix <- function(x, width = 3, direction = "backward", fill = NA) {
  fill <- ifelse(is.null(fill), NA, fill)
  value_matrix <- matrix(fill, nrow = length(x), ncol = width)
  for (i in 0:(width - 1)) {
    if (direction == "backward") {
      value_matrix[(i + 1):length(x), i + 1] <- x[1:(length(x) - i)]
    } else if (direction == "forward") {
      value_matrix[1:(length(x) - i), i + 1] <- x[(i + 1):length(x)]
    } else {
      # TODO: implement center
      stop("direction must be 'backward' or 'forward'")
    }
  }
  return(value_matrix)
}
