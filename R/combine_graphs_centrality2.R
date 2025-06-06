combine_graphs_centrality2  <- function(Figura1_Derecha, network, groups, error_Model,
                                        labels = NULL,
                                        abbreviate_labels = FALSE,
                                        ncol = 2, widths = c(0.50, 0.60),
                                        dpi = 600,
                                        legend.cex = 0.1) {
  # Verificar e instalar librerías necesarias
  required_packages <- c("ggplot2", "qgraph", "png", "grid", "patchwork", "Cairo")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Función para abreviar nombres a 3 letras
  abbreviate_names <- function(labels) {
    substr(labels, 1, 3)
  }

  # Determinar las etiquetas a usar
  final_labels <- if (is.null(labels)) network$labels else labels
  if (abbreviate_labels) {
    final_labels <- abbreviate_names(final_labels)
  }

  # Generar el gráfico de qgraph y guardarlo temporalmente como PNG
  tmp_file <- tempfile(fileext = ".png")
  Cairo::CairoPNG(tmp_file, width = 1600, height = 1000, res = dpi)
  qgraph(network$graph,
         groups = groups,
         curveAll = 2,
         vsize = 18,
         esize = 18,
         palette = "pastel",
         layout = "spring",
         edge.labels = TRUE,
         labels = final_labels,
         legend.cex = legend.cex,
         legend = TRUE,
         details = FALSE,
         node.width = 0.8,
         pie = error_Model,
         layoutScale = c(0.9, 0.9),
         GLratio = 2,
         edge.label.cex = 1)
  dev.off()

  # Leer la imagen PNG como rasterGrob
  img <- png::readPNG(tmp_file)
  g1_raster <- grid::rasterGrob(img, interpolate = TRUE)

  # Convertir el rasterGrob en un ggplot vacío con la imagen como fondo
  p1 <- ggplot2::ggplot() +
    ggplot2::annotation_custom(
      grob = g1_raster,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    ggplot2::theme_void()

  # Modificar la leyenda del gráfico de líneas y puntos
  Figura1_Derecha_modificada <- Figura1_Derecha +
    ggplot2::labs(color = "Metric", shape = "Metric") +
    ggplot2::scale_color_discrete(labels = c("Bridge EI", "EI")) +
    ggplot2::scale_shape_discrete(labels = c("Bridge EI", "EI")) +
    ggplot2::theme(
      legend.direction = "vertical",
      legend.position = "right",
      legend.box = "vertical"
    )

  # Combinar p1 (qgraph) y Figura1_Derecha_modificada usando patchwork
  combinado <- p1 + Figura1_Derecha_modificada +
    patchwork::plot_layout(ncol = ncol, widths = widths)

  # Asignar clase personalizada y definir método print para suprimir el aviso
  class(combinado) <- c("silent_gg", class(combinado))
  assign("print.silent_gg",
         function(x, ...) suppressWarnings(NextMethod()),
         envir = .GlobalEnv)

  # Eliminar archivo temporal
  unlink(tmp_file)

  # Devolver el objeto ggplot2 resultante
  return(combinado)
}
