#' Plot networks by group as a combined panel
#'
#' @param networks_by_group A named list of network objects (one per group).
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param units Units for width/height.
#' @param res Plot resolution in DPI.
#' @param groups Named list mapping factor names to node indices.
#' @param labels Optional character vector of node labels.
#' @param pie Optional named list of pie values per group.
#' @param legend.cex Legend text size.
#' @param GLratio Graph-to-legend ratio.
#' @param ncol Number of columns for the panel layout.
#' @param plot_margin Margin around each plot.
#' @param title_spacing Spacing for titles.
#' @param show_plot Logical; whether to print the plot.
#' @param layoutScale Numeric vector of length 2 for layout scaling.
#' @param vsize Node size.
#' @param esize Edge size.
#' @param node.width Node width.
#' @param edge.labels Logical; show edge labels.
#' @param edge.label.cex Edge label text size.
#' @param curveAll Edge curvature.
#' @param minimum Minimum edge weight to display.
#' @param border.width Node border width.
#' @param label.cex Node label text size.
#' @param title.cex Title text size.
#' @param palette Color palette name.
#' @param color Optional vector of node colors per group.
#'
#' @return A combined ggplot (patchwork) object.
#' @export
#' @importFrom ggplot2 ggplot annotation_raster xlim ylim theme_void theme margin
#' @importFrom patchwork wrap_plots
#' @importFrom magick image_read
#' @importFrom qgraph qgraph averageLayout
plot_networks_by_group <- function(networks_by_group,
                                   width = 7, height = 7, units = "in", res = 300,
                                   groups = list("Factor1" = 1:6, "Factor2" = 7:12),
                                   labels = NULL,
                                   pie = NULL,
                                   legend.cex = 0.5,
                                   GLratio = 1.5,
                                   ncol = NULL,
                                   plot_margin = 20,
                                   title_spacing = 6,
                                   show_plot = TRUE,
                                   # Nuevos parametros para personalizar qgraph
                                   layoutScale = c(1, 1),
                                   vsize = 12,
                                   esize = 20,
                                   node.width = 1,
                                   edge.labels = TRUE,
                                   edge.label.cex = 1.5,
                                   curveAll = 2,
                                   minimum = 0.10,
                                   border.width = 2.5,
                                   label.cex = 1,
                                   title.cex = 1.6,
                                   palette = "ggplot2",
                                   color = NULL) {

  # Obtener el layout promedio basado en el primer grupo
  L <- do.call(averageLayout, lapply(networks_by_group, function(x) x$graph))

  # Lista para almacenar los graficos ggplot2
  plot_list <- list()

  # Crear cada grafico
  for (i in seq_along(networks_by_group)) {
    group_name <- names(networks_by_group)[i]

    # Asignar valores de pie para el grupo actual, si existen
    pie_values <- if (!is.null(pie) && group_name %in% names(pie)) pie[[group_name]] else NULL

    # Crear un archivo temporal para este grafico especifico
    temp_file <- tempfile(fileext = ".png")

    # Abrir dispositivo grafico temporal
    png(filename = temp_file, width = width, height = height, units = units, res = res)

    # Configurar margenes mas amplios
    par(mar = c(2, 2, title_spacing, 2))  # bottom, left, top, right - margen superior ajustable

    # Construir vector de colores por nodo si se especifica color por grupo
    node_colors <- NULL
    if (!is.null(color)) {
      # color debe ser un vector con un color por cada grupo en 'groups'
      # Crear vector de colores para cada nodo segun su grupo
      n_nodes <- ncol(networks_by_group[[i]]$graph)
      node_colors <- rep(NA, n_nodes)
      for (g in seq_along(groups)) {
        node_colors[groups[[g]]] <- color[g]
      }
    }

    # Crear el grafico qgraph
    qgraph(networks_by_group[[i]]$graph,
           layout = L,
           palette = palette,
           groups = groups,
           color = node_colors,
           labels = if (!is.null(labels)) labels else colnames(networks_by_group[[i]]$graph),
           pie = pie_values,
           title = paste0("\n", "Group: ", group_name),
           title.cex = title.cex,
           edge.labels = edge.labels,
           edge.label.cex = edge.label.cex,
           edge.label.position = 0.5,
           border.width = border.width,
           label.cex = label.cex,
           legend.cex = legend.cex,
           legend.mode = "groups",
           GLratio = GLratio,
           vsize = vsize,
           esize = esize,
           node.width = node.width,
           curveAll = curveAll,
           minimum = minimum,
           mar = c(5, 3, 5, 3),
           rescale = TRUE,
           layoutScale = layoutScale)

    # Cerrar el dispositivo
    dev.off()

    # Leer la imagen con magick
    img <- image_read(temp_file)

    # Convertir a raster
    img_raster <- as.raster(img)

    # Crear un grafico ggplot2 con la imagen
    p <- ggplot() +
      annotation_raster(img_raster, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(plot.margin = margin(plot_margin, plot_margin, plot_margin, plot_margin))

    # Agregar a la lista
    plot_list[[i]] <- p

    # Eliminar archivo temporal
    unlink(temp_file)
  }

  # Determinar el numero de columnas si no se especifica
  if (is.null(ncol)) {
    n_groups <- length(networks_by_group)
    ncol <- ceiling(sqrt(n_groups))
  }

  # Combinar todos los graficos usando patchwork
  if (length(plot_list) == 1) {
    combined_plot <- plot_list[[1]]
  } else {
    combined_plot <- wrap_plots(plot_list, ncol = ncol)
  }

  # Mostrar el grafico si se solicita
  if (show_plot) {
    print(combined_plot)
  }

  # Retornar el objeto ggplot2 combinado
  return(combined_plot)
}
