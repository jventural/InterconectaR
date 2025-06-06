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
                                   show_plot = TRUE) {
  # Cargar las librerías necesarias
  library(ggplot2)
  library(patchwork)
  library(magick)
  library(grid)

  # Obtener el layout promedio basado en el primer grupo
  L <- do.call(averageLayout, lapply(networks_by_group, function(x) x$graph))

  # Lista para almacenar los gráficos ggplot2
  plot_list <- list()

  # Crear cada gráfico
  for (i in seq_along(networks_by_group)) {
    group_name <- names(networks_by_group)[i]

    # Asignar valores de pie para el grupo actual, si existen
    pie_values <- if (!is.null(pie) && group_name %in% names(pie)) pie[[group_name]] else NULL

    # Crear un archivo temporal para este gráfico específico
    temp_file <- tempfile(fileext = ".png")

    # Abrir dispositivo gráfico temporal
    png(filename = temp_file, width = width, height = height, units = units, res = res)

    # Configurar márgenes más amplios
    par(mar = c(2, 2, title_spacing, 2))  # bottom, left, top, right - margen superior ajustable

    # Crear el gráfico qgraph
    qgraph(networks_by_group[[i]]$graph,
           layout = L,
           palette = "ggplot2",
           groups = groups,
           labels = if (is.null(labels)) networks_by_group[[i]][["labels"]] else labels,
           pie = pie_values,
           title = paste0("\n", "Group: ", group_name),  # Añadí salto de línea antes del título
           title.cex = 1.6,  # Reduje ligeramente el tamaño
           edge.labels = TRUE,
           edge.label.cex = 1.5,
           edge.label.position = 0.5,
           border.width = 2.5,
           label.cex = 1,
           legend.cex = legend.cex,
           legend.mode = "groups",
           GLratio = GLratio,  # Ajustar la proporción del gráfico vs leyenda
           vsize = 12,
           curveAll = 2,
           minimum = 0.10,
           mar = c(5, 3, 5, 3),  # Márgenes internos del gráfico
           rescale = TRUE)

    # Cerrar el dispositivo
    dev.off()

    # Leer la imagen con magick
    img <- image_read(temp_file)

    # Convertir a raster
    img_raster <- as.raster(img)

    # Crear un gráfico ggplot2 con la imagen
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

  # Determinar el número de columnas si no se especifica
  if (is.null(ncol)) {
    n_groups <- length(networks_by_group)
    ncol <- ceiling(sqrt(n_groups))
  }

  # Combinar todos los gráficos usando patchwork
  if (length(plot_list) == 1) {
    combined_plot <- plot_list[[1]]
  } else {
    combined_plot <- wrap_plots(plot_list, ncol = ncol)
  }

  # Mostrar el gráfico si se solicita
  if (show_plot) {
    print(combined_plot)
  }

  # Retornar el objeto ggplot2 combinado
  return(combined_plot)
}
