combine_graphs_centrality <- function(Figura1_Derecha, network, groups, error_Model,
                                       labels = NULL,
                                       ncol = 2, widths = c(0.50, 0.60),
                                       output_path = "Output/Figura_1_Final.jpg",
                                       show_plot = FALSE,
                                       dpi = 600,
                                       legend.cex = 0.1) { # Nuevo argumento legend.cex con valor predeterminado
  # Verificar e instalar librerías necesarias
  required_packages <- c("ggplot2", "qgraph", "gridExtra", "png", "grid", "Cairo")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Guardar el primer gráfico (g1) como archivo PNG con la resolución especificada
  Cairo::CairoPNG("g1_temp.png", width = 1600, height = 1000, res = dpi)
  qgraph(network$graph,
         groups = groups,
         curveAll = 2,
         vsize = 18,
         esize = 18,
         palette = "pastel", # Opciones: colorblind, ggplot2, pastel, rainbow
         layout = "spring",
         edge.labels = TRUE,
         labels = if (is.null(labels)) network$labels else labels, # Uso del argumento labels
         legend.cex = legend.cex, # Uso del nuevo argumento legend.cex
         legend = TRUE,
         details = FALSE,
         node.width = 0.8,
         pie = error_Model,
         layoutScale = c(0.9, 0.9),
         GLratio = 2,
         edge.label.cex = 1)
  dev.off() # Cierra el dispositivo gráfico

  # Cargar la imagen guardada como un grob
  img <- png::readPNG("g1_temp.png")
  g1_raster <- grid::rasterGrob(img, interpolate = TRUE)

  # Mostrar o guardar el gráfico combinado
  if (show_plot) {
    # Mostrar el gráfico en la ventana de RStudio
    gridExtra::grid.arrange(g1_raster, Figura1_Derecha, ncol = ncol, widths = widths)
  } else {
    # Guardar el gráfico como un archivo JPEG con la resolución especificada
    jpeg(output_path, width = 15, height = 6.5, units = "in", res = dpi)
    gridExtra::grid.arrange(g1_raster, Figura1_Derecha, ncol = ncol, widths = widths)
    dev.off()
    message("Figure saved at: ", output_path)
  }

  # Eliminar el archivo temporal
  unlink("g1_temp.png")
}
