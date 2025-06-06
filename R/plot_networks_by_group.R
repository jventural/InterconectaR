plot_networks_by_group <- function(networks_by_group,
                                    width = 14, height = 8, units = "in", res = 300,
                                    groups = list("Factor1" = 1:6, "Factor2" = 7:12),
                                    labels = NULL,
                                    pie = NULL,
                                    legend.cex = 0.5,
                                    show_plot = FALSE) {
  library(qgraph)
  library(ggplot2)
  library(patchwork)
  library(png)
  library(grid)

  # Calcular diseño uniforme basado en el primer grupo
  L <- do.call(averageLayout, lapply(networks_by_group, function(x) x$graph))

  # Número de grupos y disposición en patchwork
  n_groups <- length(networks_by_group)
  n_rows <- ceiling(sqrt(n_groups))
  n_cols <- ceiling(n_groups / n_rows)

  # Para cada grupo, crear un ggplot que contenga el qgraph como fondo
  plots <- lapply(seq_along(networks_by_group), function(i) {
    group_name <- names(networks_by_group)[i]
    net_obj    <- networks_by_group[[i]]
    pie_values  <- if (!is.null(pie) && group_name %in% names(pie)) pie[[group_name]] else NULL

    # Crear archivo temporal PNG para qgraph
    tmp_png <- tempfile(fileext = ".png")
    png(filename = tmp_png,
        width  = width,
        height = height,
        units  = units,
        res    = res)
    qgraph(net_obj$graph,
           layout = L,
           palette = "ggplot2",
           groups = groups,
           labels = if (is.null(labels)) net_obj[["labels"]] else labels,
           pie = pie_values,
           title = paste0("Group: ", group_name),
           title.cex = 2.0,
           edge.labels = TRUE,
           edge.label.cex = 1.5,
           edge.label.position = 0.5,
           border.width = 2.5,
           label.cex = 1,
           legend.cex = legend.cex,
           vsize = 12,
           curveAll = 2,
           minimum = 0.10)
    dev.off()

    # Leer el PNG como rasterGrob
    img_raster <- png::readPNG(tmp_png)
    g_raster <- grid::rasterGrob(img_raster, interpolate = TRUE)
    unlink(tmp_png)

    # Envolver el raster en un ggplot vacío
    p <- ggplot() +
      annotation_custom(
        grob = g_raster,
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      ) +
      theme_void() +
      ggtitle(paste0("Group: ", group_name)) +
      theme(plot.title = element_text(hjust = 0.5, size = 14))

    return(p)
  })

  # Combinar todos los ggplots con patchwork
  combined <- wrap_plots(plots, ncol = n_cols)

  if (show_plot) {
    print(combined)
  }

  return(combined)
}
