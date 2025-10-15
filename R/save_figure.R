#' Save a ggplot2 figure for publishing
#'
#' @param gg a single ggplot object you would like to save.
#' @param out_path A single character value indicating where to save the plot to (i.e. "./plots/my_figure_name.png")
#' @param page_width A single numeric value indicating the width of the page (minus margins) that the image is intended for.
#'   Default is 6.5".
#' @param base_height A single numeric value indicating the height of the figure (+`taller`).
#'   Default is 5".
#' @param taller A single numeric value indicating how much to add/remove to the plot `base_height`.
#'   Default is 0.
#' @param units A single character value equal to "in" (default) or "cm" indicating the units of `page_width`, `base_height`, and `taller`.
#' @param quality A single character value equal to "high" (dpi = 300), "medium" (dpi = 200), or "low" (dpi = 100) indicating the output quality of the figure.
#'   OR a single numeric value equal to the desired dpi.
#'   Text sizes may need to be adjusted in relavant geoms/themes for different quality levels.
#' @param ... (Optional) addition arguments passed on to ggplot2::ggsave()
#'
#' @description
#' `save_figure()` is a wrapper around `ggplot2::ggsave()` that saves a high quality 5x6.5 figure by default.
#' You can adjust the sizing from there by changing `taller` and `page_width`.
#'
#' @family Data Visualisation
#'
#' @return invisibly returns `gg`
#' @export
#'
#' @examples
#' gg <- ggplot2::ggplot() +
#'   ggplot2::geom_line(
#'     data = data.frame(x = 1:10, y = (0:9)^2),
#'     ggplot2::aes(x, y)
#'   )
#' # save_figure(gg, "./test.png", taller = 1)
save_figure <- function(
  gg,
  out_path,
  page_width = 6.5,
  base_height = 5,
  taller = 0,
  units = "in",
  quality = "high",
  ...
) {
  rlang::check_installed("ggplot2")
  # Handle inputs
  stopifnot(inherits(gg, "gg"))
  stopifnot(is.character(out_path), length(out_path) == 1)
  stopifnot(is.numeric(page_width), length(page_width) == 1)
  stopifnot(is.numeric(base_height), length(base_height) == 1)
  stopifnot(is.numeric(taller), length(taller) == 1)
  stopifnot(is.character(units), length(units) == 1)
  stopifnot(
    quality %in% c("high", "medium", "low") | is.numeric(quality),
    length(quality) == 1
  )

  # translate quality -> dpi
  dpi <- if (quality == "high") {
    300
  } else if (quality == "medium") {
    200
  } else if (quality == "low") {
    100
  } else {
    quality
  }

  # save figure
  ggplot2::ggsave(
    filename = out_path,
    plot = gg,
    width = page_width,
    height = base_height + taller,
    units = units,
    dpi = dpi,
    ...
  )
  # return plot object invisibly in case desired for later
  invisible(gg)
}
