plot_latent_network_diagram <- function(model_object,
                                        filename,
                                        output_path = ".",
                                        width = 9.5,
                                        height = 6.5,
                                        resolution = 1500,
                                        node_labels = NULL,
                                        color_scheme = NULL,
                                        color_palette = "auto",
                                        plot_settings = NULL,
                                        device_settings = NULL,
                                        edge_settings = NULL,
                                        save_message = TRUE,
                                        show_manifest = TRUE) {

  # Paquetes requeridos
  if (!require("semPlot", quietly = TRUE)) {
    stop("El paquete 'semPlot' es requerido. Instálalo para continuar.")
  }
  if (!require("RColorBrewer", quietly = TRUE)) {
    warning("RColorBrewer no disponible. Usaré colores por defecto.")
  }

  # Null-coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Extraer matrices según clase del modelo
  if (inherits(model_object, "psychonetrics")) {
    lambda_est <- getmatrix(model_object, "lambda")
    psi_est    <- getmatrix(model_object, "sigma_zeta")
    theta_est  <- getmatrix(model_object, "sigma_epsilon")
  } else if (inherits(model_object, "lavaan")) {
    lambda_est <- lavaan::inspect(model_object, "std")$lambda
    psi_est    <- lavaan::inspect(model_object, "std")$psi
    theta_est  <- lavaan::inspect(model_object, "std")$theta
  } else {
    lambda_est <- getmatrix(model_object, "lambda")
    psi_est    <- getmatrix(model_object, "sigma_zeta")
    theta_est  <- getmatrix(model_object, "sigma_epsilon")
  }

  # Dimensiones
  n_manifest <- nrow(lambda_est)
  n_latent   <- ncol(lambda_est)

  # Modelo LISREL
  mod <- semPlot::lisrelModel(LY = lambda_est, PS = psi_est, TE = theta_est)

  # Ruta de salida
  output_file <- file.path(output_path, filename)
  output_dir  <- dirname(output_file)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Dispositivo gráfico (por defecto, sobreescribible)
  default_device <- list(width = width, height = height, units = "in", res = resolution)
  device_settings <- if (is.null(device_settings)) default_device else modifyList(default_device, device_settings)

  # Ajustes de plot por defecto
  default_plot <- list(
    what = "std", layout = "circle", curve = 0.5, curveAdjacent = TRUE,
    sizeMan = 1.5, sizeMan2 = 1.5, sizeLat = 12, layoutSplit = TRUE,
    open = FALSE, intercepts = FALSE, residuals = FALSE, thresholds = FALSE
  )
  plot_settings <- if (is.null(plot_settings)) default_plot else modifyList(default_plot, plot_settings)

  # Permitir que nodeLabels.cex funcione (mapea a label.cex de semPaths)
  if (!is.null(plot_settings$nodeLabels.cex)) {
    plot_settings$label.cex <- plot_settings$nodeLabels.cex
    plot_settings$nodeLabels.cex <- NULL
  }

  # Ajustes de aristas por defecto
  default_edge <- list(edge.label.cex = 0.8, edge.label.color = "black")
  edge_settings <- if (is.null(edge_settings)) default_edge else modifyList(default_edge, edge_settings)

  # Generador de colores
  generate_color_scheme <- function(n_manifest, n_latent, palette_type) {
    if (palette_type == "auto") {
      return(list(man = rep("#3498DB", n_manifest), lat = rep("#E74C3C", n_latent)))
    }
    use_brewer <- requireNamespace("RColorBrewer", quietly = TRUE)

    if (palette_type == "brewer_set1" && use_brewer) {
      total_colors <- min(max(n_manifest, n_latent, 3), 9)
      colors <- RColorBrewer::brewer.pal(total_colors, "Set1")
      return(list(man = rep(colors[1], n_manifest), lat = rep(colors[2], n_latent)))
    }

    if (palette_type == "brewer_set2" && use_brewer) {
      total_colors <- min(max(n_manifest, n_latent, 3), 8)
      colors <- RColorBrewer::brewer.pal(total_colors, "Set2")
      return(list(man = rep(colors[1], n_manifest), lat = rep(colors[2], n_latent)))
    }

    if (palette_type == "brewer_mixed" && use_brewer) {
      man_colors <- if (n_manifest <= 8) {
        RColorBrewer::brewer.pal(max(3, n_manifest), "Set2")[1:min(n_manifest, 8)]
      } else rep(RColorBrewer::brewer.pal(8, "Set2"), length.out = n_manifest)

      lat_colors <- if (n_latent <= 9) {
        RColorBrewer::brewer.pal(max(3, n_latent), "Set1")[1:min(n_latent, 9)]
      } else rep(RColorBrewer::brewer.pal(9, "Set1"), length.out = n_latent)

      return(list(man = man_colors, lat = lat_colors))
    }

    if (palette_type == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
      return(list(
        man = viridis::viridis(n_manifest, option = "D"),
        lat = viridis::viridis(n_latent, option = "C")
      ))
    }

    if (palette_type == "rainbow") {
      return(list(man = rainbow(n_manifest), lat = rainbow(n_latent, start = 0.5)))
    }

    if (palette_type == "heat") {
      return(list(man = heat.colors(n_manifest), lat = terrain.colors(n_latent)))
    }

    if (palette_type == "custom_blue_red") {
      blue_palette <- colorRampPalette(c("#E3F2FD", "#1976D2"))(n_manifest)
      red_palette  <- colorRampPalette(c("#FFEBEE", "#C62828"))(n_latent)
      return(list(man = blue_palette, lat = red_palette))
    }

    if (palette_type == "pastel") {
      pastel_colors <- c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFD4BA",
                         "#E0BBE4", "#C7CEEA", "#FFC1CC", "#B5EAD7", "#FFDAB9")
      return(list(
        man = rep(pastel_colors, length.out = n_manifest),
        lat = rep(rev(pastel_colors), length.out = n_latent)
      ))
    }

    # Fallback
    list(man = rep("#3498DB", n_manifest), lat = rep("#E74C3C", n_latent))
  }

  # Etiquetas por defecto (manifiestos + latentes)
  default_labels <- c(
    rownames(lambda_est) %||% paste0("Item", seq_len(n_manifest)),
    colnames(lambda_est) %||% paste0("Factor", seq_len(n_latent))
  )

  if (is.null(node_labels)) {
    node_labels <- default_labels
  } else {
    # Si solo pasaron etiquetas para latentes, completa con las de manifiestos
    if (length(node_labels) == n_latent) {
      node_labels <- c(rownames(lambda_est) %||% paste0("Item", seq_len(n_manifest)),
                       node_labels)
    } else if (length(node_labels) == n_manifest) {
      node_labels <- c(node_labels,
                       colnames(lambda_est) %||% paste0("Factor", seq_len(n_latent)))
    } else if (length(node_labels) != (n_manifest + n_latent)) {
      warning(sprintf(
        "node_labels tiene longitud %d, pero se esperaban %d. Se usarán etiquetas por defecto.",
        length(node_labels), n_manifest + n_latent))
      node_labels <- default_labels
    }
  }

  # Colores
  auto_colors <- generate_color_scheme(n_manifest, n_latent, color_palette)
  if (is.null(color_scheme)) color_scheme <- auto_colors
  # Completar faltantes y ajustar longitudes
  if (is.null(color_scheme$man)) color_scheme$man <- auto_colors$man
  if (is.null(color_scheme$lat)) color_scheme$lat <- auto_colors$lat
  color_scheme$man <- rep(color_scheme$man, length.out = n_manifest)
  color_scheme$lat <- rep(color_scheme$lat, length.out = n_latent)

  # Abrir dispositivo
  file_ext <- tools::file_ext(output_file)
  switch(tolower(file_ext),
         "jpg" = , "jpeg" = jpeg(output_file, width = device_settings$width, height = device_settings$height,
                                 units = device_settings$units, res = device_settings$res),
         "png" = png(output_file, width = device_settings$width, height = device_settings$height,
                     units = device_settings$units, res = device_settings$res),
         "pdf" = pdf(output_file, width = device_settings$width, height = device_settings$height),
         stop("Formato no soportado. Usa .jpg, .png o .pdf")
  )

  # ---------- Parches clave ----------
  if (show_manifest) {
    # Si el usuario accidentalmente puso sizeMan=0, forzar a > 0
    plot_settings$sizeMan  <- max(1, plot_settings$sizeMan  %||% 1.5)
    plot_settings$sizeMan2 <- max(1, plot_settings$sizeMan2 %||% 1.5)

    # Reducir un poco los labels de aristas para no saturar
    modified_edge_settings <- edge_settings
    modified_edge_settings$edge.label.cex <- modified_edge_settings$edge.label.cex %||% 0.8
    modified_edge_settings$edge.label.cex <- min(0.8, modified_edge_settings$edge.label.cex)

    sem_args <- c(
      list(object = mod, nodeLabels = node_labels, color = color_scheme),
      plot_settings,
      modified_edge_settings
    )
    do.call(semPlot::semPaths, sem_args)

  } else {
    # Solo latentes: ocultar por completo los manifiestos
    latent_labels <- tail(node_labels, n_latent)

    modified_color_scheme <- color_scheme
    modified_color_scheme$man <- rep("white", n_manifest)

    semPlot::semPaths(
      object = mod,
      nodeLabels = c(rep("", n_manifest), latent_labels),
      color = modified_color_scheme,
      what = plot_settings$what %||% "std",
      layout = plot_settings$layout %||% "circle",
      curve = plot_settings$curve %||% 0.5,
      curveAdjacent = plot_settings$curveAdjacent %||% TRUE,
      sizeMan = 0,
      sizeMan2 = 0,
      sizeLat = plot_settings$sizeLat %||% 12,
      layoutSplit = plot_settings$layoutSplit %||% TRUE,
      open = plot_settings$open %||% FALSE,
      intercepts = FALSE,
      residuals = FALSE,
      thresholds = FALSE,
      edge.label.cex = edge_settings$edge.label.cex %||% 0.8,
      edge.label.color = edge_settings$edge.label.color %||% "black"
    )
  }
  dev.off()

  if (save_message) cat("SEM plot guardado en:", output_file, "\n")
}
