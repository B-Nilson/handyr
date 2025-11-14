# Save a ggplot2 figure for publishing

`save_figure()` is a wrapper around
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
that saves a high quality 5x6.5 figure by default. You can adjust the
sizing from there by changing `taller` and `page_width`.

## Usage

``` r
save_figure(
  gg,
  out_path,
  page_width = 6.5,
  base_height = 5,
  taller = 0,
  units = "in",
  quality = "high",
  ...
)
```

## Arguments

- gg:

  a single ggplot object you would like to save.

- out_path:

  A single character value indicating where to save the plot to (i.e.
  "./plots/my_figure_name.png")

- page_width:

  A single numeric value indicating the width of the page (minus
  margins) that the image is intended for. Default is 6.5".

- base_height:

  A single numeric value indicating the height of the figure
  (+`taller`). Default is 5".

- taller:

  A single numeric value indicating how much to add/remove to the plot
  `base_height`. Default is 0.

- units:

  A single character value equal to "in" (default) or "cm" indicating
  the units of `page_width`, `base_height`, and `taller`.

- quality:

  A single character value equal to "high" (dpi = 300), "medium" (dpi =
  200), or "low" (dpi = 100) indicating the output quality of the
  figure. OR a single numeric value equal to the desired dpi. Text sizes
  may need to be adjusted in relavant geoms/themes for different quality
  levels.

- ...:

  (Optional) addition arguments passed on to ggplot2::ggsave()

## Value

invisibly returns `gg`

## Examples

``` r
gg <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = data.frame(x = 1:10, y = (0:9)^2),
    ggplot2::aes(x, y)
  )
# save_figure(gg, "./test.png", taller = 1)
```
