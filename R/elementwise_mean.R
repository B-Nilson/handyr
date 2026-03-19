#' Vectorized element-wise mean, optionally ignoring NA values
#'
#' @param ... Vectors to compute the element-wise mean of. Must all be the same length.
#' @param digits Number of digits to round to (optional, default is `NULL` for no rounding).
#' @param na.rm If TRUE, NA values are ignored.
#' @return Vector of the element-wise mean of `...` with the same length as vectors provided.
#' @export
elementwise_mean <- function(..., digits = NULL, na.rm = FALSE) {
  stopifnot(
    all(lengths(list(...)) == length(list(...)[[1]])),
    length(list(...)) > 0,
    (is.numeric(digits) & length(digits) == 1) | is.null(digits),
    is.logical(na.rm),
    length(na.rm) == 1
  )

  dots_list <- list(...)
  if (na.rm) {
    n <- dots_list |>
      lapply(\(x) as.integer(!is.na(x))) |> # don't count NA's in total
      do.call(what = `+`)
    dots_list <- dots_list |> lapply(swap, what = NA, with = 0)
  } else {
    n <- rep(length(dots_list), times = length(dots_list[[1]]))
  }
  means <- (do.call(dots_list, what = `+`) / n) |>
    swap(c(Inf, NaN), with = NA)
  if (!is.null(digits)) {
    means <- means |> round(digits = digits)
  }
  return(means)
}
