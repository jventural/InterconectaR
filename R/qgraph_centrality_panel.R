qgraph_centrality_panel <- function(Figura1_Derecha, network, groups, error_Model,
                                    labels = NULL,
                                    abbreviate_labels = FALSE,
                                    ncol = 2, widths = c(0.50, 0.60),
                                    dpi = 600,
                                    legend.cex = 0.1,
                                    legend_title = "Métrica",
                                    keep_centrality_scale = TRUE) {

  # Cargar librerías (no recomendado instalar dentro de función en paquetes)
  required_packages <- c("ggplot2", "qgraph", "png", "grid", "patchwork", "Cairo")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  abbreviate_names <- function(x) substr(x, 1, 3)

  final_labels <- if (is.null(labels)) network$labels else labels
  if (abbreviate_labels) final_labels <- abbreviate_names(final_labels)

  tmp_file <- tempfile(fileext = ".png")
  on.exit({ if (file.exists(tmp_file)) unlink(tmp_file) }, add = TRUE)

  # Render qgraph a PNG (panel izquierdo)
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

  img <- png::readPNG(tmp_file)
  g1_raster <- grid::rasterGrob(img, interpolate = TRUE)

  p1 <- ggplot2::ggplot() +
    ggplot2::annotation_custom(grob = g1_raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::theme_void()

  # Mantener escalas originales del plot derecho (no tocar scale_* de Figura1_Derecha)
  Figura1_Derecha_modificada <- Figura1_Derecha +
    ggplot2::labs(color = legend_title, shape = legend_title)

  # Combinar y COLECTAR leyendas; ubicarlas ABAJO con plot_annotation(theme=...)
  combinado <- p1 + Figura1_Derecha_modificada +
    patchwork::plot_layout(ncol = ncol, widths = widths, guides = "collect") +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.box       = "horizontal",
        legend.title     = ggplot2::element_text(size = 12, face = "bold"),
        legend.text      = ggplot2::element_text(size = 11)
      )
    )

  # Silenciar warnings al imprimir (opcional)
  class(combinado) <- c("silent_gg", class(combinado))
  assign("print.silent_gg", function(x, ...) suppressWarnings(NextMethod()), envir = .GlobalEnv)

  return(combinado)
}
