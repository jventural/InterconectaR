#' Create a combined panel with qgraph network and centrality plot
#'
#' @param Figura1_Derecha A ggplot object for the right panel (centrality plot).
#' @param network A network object with elements \code{graph} and \code{labels}.
#' @param groups Named list mapping factor names to node indices.
#' @param error_Model Pie values for error model visualization.
#' @param labels Optional character vector of node labels.
#' @param abbreviate_labels Logical; abbreviate labels to 3 characters.
#' @param ncol Number of columns in the layout.
#' @param widths Relative widths of the panels.
#' @param dpi Resolution in DPI for the qgraph rendering.
#' @param legend.cex Legend text size for qgraph.
#' @param legend_title Title for the legend.
#' @param keep_centrality_scale Logical; keep original scale of centrality plot.
#' @param layoutScale Numeric vector of length 2 for layout scaling.
#' @param GLratio Graph-to-legend ratio.
#' @param vsize Node size.
#' @param esize Edge size.
#' @param node.width Node width.
#' @param curveAll Edge curvature.
#' @param palette Color palette name.
#' @param layout Layout algorithm.
#' @param edge.labels Logical; show edge labels.
#' @param edge.label.cex Edge label text size.
#'
#' @return A combined patchwork plot object.
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
#' qgraph_centrality_panel(cent$plot, net, groups = list(A = 1:4, B = 5:8),
#'                         error_Model = r2, dpi = 100)
#' @importFrom ggplot2 ggplot annotation_custom theme_void labs theme element_text
#' @importFrom qgraph qgraph
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom Cairo CairoPNG
qgraph_centrality_panel <- function(Figura1_Derecha, network, groups, error_Model,
                                    labels = NULL,
                                    abbreviate_labels = FALSE,
                                    ncol = 2, widths = c(0.50, 0.60),
                                    dpi = 600,
                                    legend.cex = 0.1,
                                    legend_title = "M\u00e9trica",
                                    keep_centrality_scale = TRUE,
                                    # Parametros de qgraph
                                    layoutScale = c(0.9, 0.9),
                                    GLratio = 2,
                                    vsize = 18,
                                    esize = 18,
                                    node.width = 0.8,
                                    curveAll = 2,
                                    palette = "pastel",
                                    layout = "spring",
                                    edge.labels = TRUE,
                                    edge.label.cex = 1) {

  abbreviate_names <- function(x) substr(x, 1, 3)

  final_labels <- if (is.null(labels)) network$labels else labels
  if (abbreviate_labels) final_labels <- abbreviate_names(final_labels)

  tmp_file <- tempfile(fileext = ".png")
  on.exit({ if (file.exists(tmp_file)) unlink(tmp_file) }, add = TRUE)

  # Render qgraph a PNG (panel izquierdo)
  Cairo::CairoPNG(tmp_file, width = 1600, height = 1000, res = dpi)
  qgraph(network$graph,
         groups = groups,
         curveAll = curveAll,
         vsize = vsize,
         esize = esize,
         palette = palette,
         layout = layout,
         edge.labels = edge.labels,
         labels = final_labels,
         legend.cex = legend.cex,
         legend = TRUE,
         details = FALSE,
         node.width = node.width,
         pie = error_Model,
         layoutScale = layoutScale,
         GLratio = GLratio,
         edge.label.cex = edge.label.cex)
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

  return(combinado)
}
