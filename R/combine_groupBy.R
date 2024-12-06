combine_groupBy <- function(red_group, plot_centralidad_group, bridge_plot_group = NULL,
                             output_path = "combine_three_graphs.jpg",
                             height = 10, width_a = 8, width_bc = 4.5,
                             dpi = 300,
                             show_plot = TRUE) {
  # Verificar e instalar librerías necesarias
  required_packages <- c("ggplot2", "gridExtra", "grid", "Cairo", "magick", "cowplot")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Convertir el gráfico red_group en un objeto raster (imagen)
  temp_file <- tempfile(fileext = ".png")
  magick::image_write(red_group, path = temp_file)
  p12_img <- png::readPNG(temp_file)
  p12_raster <- grid::rasterGrob(p12_img, interpolate = TRUE)

  # Crear etiquetas para identificar cada subgráfico
  label_a <- cowplot::ggdraw() +
    cowplot::draw_label("A", fontface = "bold", size = 14, x = 0.02, hjust = 0) +
    theme_void()
  label_b <- cowplot::ggdraw() +
    cowplot::draw_label("B", fontface = "bold", size = 14, x = 0.02, hjust = 0) +
    theme_void()
  label_c <- cowplot::ggdraw() +
    cowplot::draw_label("C", fontface = "bold", size = 14, x = 0.02, hjust = 0) +
    theme_void()

  # Ajustar tamaños de los gráficos B y C
  b_plot <- cowplot::ggdraw(plot_centralidad_group) + theme(plot.margin = margin(5, 5, 5, 5))
  c_plot <- if (!is.null(bridge_plot_group)) {
    cowplot::ggdraw(bridge_plot_group) + theme(plot.margin = margin(5, 5, 5, 5))
  } else {
    cowplot::ggdraw() + theme_void() # Marcador de posición vacío para evitar errores
  }

  # Determinar alturas y proporciones dinámicas dependiendo de si bridge_plot_group es NULL
  if (is.null(bridge_plot_group)) {
    # Cuando bridge_plot_group es NULL: proporciones 50%-25%
    right_col <- gridExtra::arrangeGrob(
      gridExtra::arrangeGrob(label_b, b_plot, ncol = 1, heights = c(0.1, 0.9))
    )
    widths <- c(2, 1) # Red ocupa 2/3, centralidad ocupa 1/3
  } else {
    # Cuando bridge_plot_group no es NULL: proporciones normales (50%-50% dentro de la derecha)
    right_col <- gridExtra::arrangeGrob(
      gridExtra::arrangeGrob(label_b, b_plot, ncol = 1, heights = c(0.1, 0.9)),
      gridExtra::arrangeGrob(label_c, c_plot, ncol = 1, heights = c(0.1, 0.9)),
      nrow = 2, # Dos filas para B y C
      heights = c(0.5, 0.5) # Mitad y mitad
    )
    widths <- c(width_a, width_bc) # Proporciones normales
  }

  # Combinar los gráficos con las etiquetas
  left_col <- gridExtra::arrangeGrob(
    label_a,
    p12_raster,
    ncol = 1,
    heights = c(0.05, 0.95) # Etiqueta y gráfico completo
  )

  combined_plot <- gridExtra::grid.arrange(
    left_col,
    right_col,
    ncol = 2,
    widths = widths # Ajustar anchos de las columnas
  )

  # Mostrar o guardar el gráfico combinado
  if (show_plot) {
    grid::grid.newpage()
    grid::grid.draw(combined_plot)
  } else {
    jpeg(output_path, width = (width_a + width_bc), height = height, units = "in", res = dpi)
    grid::grid.draw(combined_plot)
    dev.off()
    message("Figure saved at: ", output_path)
  }

  # Eliminar el archivo temporal
  unlink(temp_file)
}
