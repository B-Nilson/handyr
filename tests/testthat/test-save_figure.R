test_that("basic case works", {
  gg <- ggplot2::ggplot(data.frame(x = 1:10, y = (0:9)^2)) +
    ggplot2::geom_line(ggplot2::aes(x, y))
  expect_silent(
    gg |> save_figure(
      out_path = tempfile(fileext = ".png"), 
      quality = "low", base_height = 1, page_width = 1
    )
  )
})
