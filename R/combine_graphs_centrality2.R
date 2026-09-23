#' Combine Network and Centrality Graphs (Version 2)
#'
#' Combines a qgraph network plot with a centrality line plot.
#'
#' @param Figura1_Derecha ggplot object with centrality plot.
#' @param network Estimated network object.
#' @param groups List of node communities.
#' @param error_Model Prediction error values for pie chart overlay.
#' @param labels Custom node labels.
#' @param abbreviate_labels Logical; abbreviate labels to 3 characters.
#' @param ncol Number of columns in layout.
#' @param widths Relative widths of panels.
#' @param dpi Resolution in dots per inch.
#' @param legend.cex Legend text size multiplier.
#'
#' @return A patchwork object with the network on the left and the
#'   centrality plot on the right.
#' @export
#' @examples
#' set.seed(123)
#' n <- 300
#' f1 <- rnorm(n)
#' f2 <- 0.3 * f1 + rnorm(n)
#' items <- data.frame(
#'   sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'   sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#' )
#' names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
#'
#' net <- bootnet::estimateNetwork(items, default = "EBICglasso")
#' g <- qgraph::qgraph(net$graph, DoNotPlot = TRUE)
#' cent <- centrality_plots2(g, net,
#'                           groups = rep(c("A", "B"), each = 4),
#'                           measure1 = "Bridge Expected Influence (1-step)")
#' r2 <- mgm_error_metrics(items, type = rep("g", 8), level = rep(1, 8))$R2
#' combine_graphs_centrality2(cent$plot, net, groups = list(A = 1:4, B = 5:8),
#'                            error_Model = r2, dpi = 100)
#' @importFrom ggplot2 ggplot annotation_custom theme_void labs scale_color_discrete scale_shape_discrete theme
#' @importFrom qgraph qgraph
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @importFrom patchwork plot_layout
#' @importFrom Cairo CairoPNG
combine_graphs_centrality2  <- function(Figura1_Derecha, network, groups, error_Model,
                                        labels = NULL,
                                        abbreviate_labels = FALSE,
                                        ncol = 2, widths = c(0.50, 0.60),
                                        dpi = 600,
                                        legend.cex = 0.1) {
  # Funcion para abreviar nombres a 3 letras
  abbreviate_names <- function(labels) {
    substr(labels, 1, 3)
  }

  # Determinar las etiquetas a usar
  final_labels <- if (is.null(labels)) network$labels else labels
  if (abbreviate_labels) {
    final_labels <- abbreviate_names(final_labels)
  }

  # Generar el grafico de qgraph y guardarlo temporalmente como PNG
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

  # Convertir el rasterGrob en un ggplot vacio con la imagen como fondo
  p1 <- ggplot2::ggplot() +
    ggplot2::annotation_custom(
      grob = g1_raster,
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    ) +
    ggplot2::theme_void()

  # Modificar la leyenda del grafico de lineas y puntos
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

  # Asignar clase personalizada y definir metodo print para suprimir el aviso
  class(combinado) <- c("silent_gg", class(combinado))

  # Eliminar archivo temporal
  unlink(tmp_file)

  # Devolver el objeto ggplot2 resultante
  return(combinado)
}
