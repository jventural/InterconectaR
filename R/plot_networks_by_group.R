plot_networks_by_group <- function(networks_by_group,
                                   output_dir = "Figuras",
                                   width = 14, height = 8, units = "in", res = 1000,
                                   groups = list("Factor1" = 1:6, "Factor2" = 7:12),
                                   labels = NULL,
                                   pie = NULL,
                                   legend.cex = 0.5,
                                   show_plot = F) {
  # Instalar la librería magick si no está disponible
  library(magick)
  # Obtener el layout promedio basado en el primer grupo
  L <- do.call(averageLayout, lapply(networks_by_group, function(x) x$graph))

  # Crear el directorio de salida si no existe
  if (!file.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Crear un archivo gráfico virtual
  temp_file <- tempfile(fileext = ".png")
  png(filename = temp_file, width = width, height = height, units = units, res = res)

  # Determinar el número de gráficos y ajustar la disposición
  n_groups <- length(networks_by_group)
  n_rows <- ceiling(sqrt(n_groups))
  n_cols <- ceiling(n_groups / n_rows)

  # Configurar el panel de gráficos
  par(mfrow = c(n_rows, n_cols))

  for (i in seq_along(networks_by_group)) {
    group_name <- names(networks_by_group)[i]

    # Asignar valores de pie para el grupo actual, si existen
    pie_values <- if (!is.null(pie) && group_name %in% names(pie)) pie[[group_name]] else NULL

    qgraph(networks_by_group[[i]]$graph,
           layout = L,
           palette = "ggplot2",
           groups = groups,
           labels = if (is.null(labels)) networks_by_group[[i]][["labels"]] else labels,
           pie = pie_values, # Valores de pastel específicos para el grupo
           title = paste0("Group: ", group_name),
           title.cex = 2.0,  # Tamaño del título
           edge.labels = TRUE,
           edge.label.cex = 1.5,
           edge.label.position = 0.5,
           border.width = 2.5,
           label.cex = 1,
           legend.cex = legend.cex,
           vsize = 12,
           curveAll = 2,
           minimum = 0.10)
  }

  # Cierra el dispositivo gráfico
  dev.off()

  # Leer la imagen guardada en el objeto
  img <- magick::image_read(temp_file)

  # Mostrar el gráfico si se solicita
  if (show_plot) {
    print(img)
  }

  # Retornar el objeto de imagen
  return(img)
}
