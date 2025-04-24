#' Save a ggplot2 figure
#'
#' @param gg a single ggplot object you would like to save.
#' @param out_path A single character value indicating where to save the plot to (i.e. "./plots/my_figure_name.png")
#' @param taller A single numeric value indicating how many inches to add/remove to the output plot height. Default makes a 5"x6.5" image.
#' @param page_width A single numeric value indicating the width of the page (minus margins) in inches that the image is intended for.
#' @param quality A single character value equal to "high" (dpi = 300), "medium" (dpi = 200), or "low" (dpi = 100) indicating the output quality of the figure.
#'   OR a single numeric value equal to the desired dpi.
#'   Text sizes may need to be adjusted in relavant geoms/themes for different quality levels.
#' @param ... (Optional) addition arguments passed on to ggplot2::ggsave()
#'
#' @description
#' `save_figure()` is a wrapper around `ggplot2::ggsave()` that saves a high quality 5x7 figure by default.
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
#'     data = data.frame(x = 1:100, y = (0:99)^2),
#'     ggplot2::aes(x, y)
#'   )
#' # save_figure(gg, "./test.png", taller = 1)
save_figure <- function(gg, out_path, taller = 0, page_width = 6.5, quality = "high", ...) {
  dpi <- if (quality == "high") 300 else if (quality == "medium") 200 else if (quality == "low") 100 else quality
  ggplot2::ggsave(
    filename = out_path,
    plot = gg,
    width = page_width,
    height = 5 + taller,
    units = "in",
    dpi = dpi,
    ...
  )
  invisible(gg)
}
