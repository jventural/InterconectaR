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

  # Load required packages
  if (!require("semPlot", quietly = TRUE)) {
    stop("semPlot package is required. Please install it.")
  }
  if (!require("RColorBrewer", quietly = TRUE)) {
    warning("RColorBrewer not available. Using default colors.")
  }

  # Helper function for null coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Extract matrices based on model type
  if (inherits(model_object, "psychonetrics")) {
    lambda_est <- getmatrix(model_object, "lambda")
    psi_est <- getmatrix(model_object, "sigma_zeta")
    theta_est <- getmatrix(model_object, "sigma_epsilon")
  } else if (inherits(model_object, "lavaan")) {
    lambda_est <- lavaan::inspect(model_object, "std")$lambda
    psi_est <- lavaan::inspect(model_object, "std")$psi
    theta_est <- lavaan::inspect(model_object, "std")$theta
  } else {
    # Try generic approach
    lambda_est <- getmatrix(model_object, "lambda")
    psi_est <- getmatrix(model_object, "sigma_zeta")
    theta_est <- getmatrix(model_object, "sigma_epsilon")
  }

  # Create LISREL model
  mod <- semPlot::lisrelModel(LY = lambda_est, PS = psi_est, TE = theta_est)

  # Create complete output file path
  output_file <- file.path(output_path, filename)

  # Default device settings (can be overridden by device_settings parameter)
  default_device <- list(width = width, height = height, units = 'in', res = resolution)
  device_settings <- if (is.null(device_settings)) default_device else modifyList(default_device, device_settings)

  # Default plot settings
  default_plot <- list(
    what = "std", layout = "circle", curve = 0.5, curveAdjacent = TRUE,
    sizeMan = 1.5, sizeMan2 = 1.5, sizeLat = 12, layoutSplit = TRUE,
    open = FALSE, intercepts = FALSE, residuals = FALSE, thresholds = FALSE
  )

  plot_settings <- if (is.null(plot_settings)) default_plot else modifyList(default_plot, plot_settings)

  # Default edge settings
  default_edge <- list(edge.label.cex = 0.8, edge.label.color = "black")
  edge_settings <- if (is.null(edge_settings)) default_edge else modifyList(default_edge, edge_settings)

  # Auto-generate labels if not provided
  if (is.null(node_labels)) {
    n_manifest <- nrow(lambda_est)
    n_latent <- ncol(lambda_est)
    node_labels <- c(paste0("Item", 1:n_manifest), paste0("Factor", 1:n_latent))
  }

  # Function to generate colors using various palettes
  generate_color_scheme <- function(n_manifest, n_latent, palette_type) {

    if (palette_type == "auto") {
      # Default simple colors
      return(list(
        man = rep("#3498DB", n_manifest),
        lat = rep("#E74C3C", n_latent)
      ))
    }

    # Check if RColorBrewer is available
    use_brewer <- requireNamespace("RColorBrewer", quietly = TRUE)

    if (palette_type == "brewer_set1") {
      if (use_brewer) {
        # Get total colors needed
        total_colors <- max(n_manifest, n_latent, 3)  # Minimum 3 for Set1
        if (total_colors > 9) total_colors <- 9  # Set1 max is 9
        colors <- RColorBrewer::brewer.pal(total_colors, "Set1")
        return(list(
          man = rep(colors[1], n_manifest),
          lat = rep(colors[2], n_latent)
        ))
      }
    }

    if (palette_type == "brewer_set2") {
      if (use_brewer) {
        total_colors <- max(n_manifest, n_latent, 3)
        if (total_colors > 8) total_colors <- 8
        colors <- RColorBrewer::brewer.pal(total_colors, "Set2")
        return(list(
          man = rep(colors[1], n_manifest),
          lat = rep(colors[2], n_latent)
        ))
      }
    }

    if (palette_type == "brewer_mixed") {
      if (use_brewer) {
        # Use different Brewer palettes for manifest and latent
        man_colors <- if (n_manifest <= 8) {
          RColorBrewer::brewer.pal(max(3, n_manifest), "Set2")[1:min(n_manifest, 8)]
        } else {
          rep(RColorBrewer::brewer.pal(8, "Set2"), length.out = n_manifest)
        }

        lat_colors <- if (n_latent <= 9) {
          RColorBrewer::brewer.pal(max(3, n_latent), "Set1")[1:min(n_latent, 9)]
        } else {
          rep(RColorBrewer::brewer.pal(9, "Set1"), length.out = n_latent)
        }

        return(list(man = man_colors, lat = lat_colors))
      }
    }

    if (palette_type == "viridis") {
      if (requireNamespace("viridis", quietly = TRUE)) {
        man_colors <- viridis::viridis(n_manifest, option = "D")
        lat_colors <- viridis::viridis(n_latent, option = "C")
        return(list(man = man_colors, lat = lat_colors))
      }
    }

    if (palette_type == "rainbow") {
      return(list(
        man = rainbow(n_manifest),
        lat = rainbow(n_latent, start = 0.5)
      ))
    }

    if (palette_type == "heat") {
      return(list(
        man = heat.colors(n_manifest),
        lat = terrain.colors(n_latent)
      ))
    }

    if (palette_type == "custom_blue_red") {
      blue_palette <- colorRampPalette(c("#E3F2FD", "#1976D2"))(n_manifest)
      red_palette <- colorRampPalette(c("#FFEBEE", "#C62828"))(n_latent)
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

    # Fallback to auto if palette not recognized
    return(list(
      man = rep("#3498DB", n_manifest),
      lat = rep("#E74C3C", n_latent)
    ))
  }

  # Auto-generate colors if not provided
  if (is.null(color_scheme)) {
    n_manifest <- nrow(lambda_est)
    n_latent <- ncol(lambda_est)
    color_scheme <- generate_color_scheme(n_manifest, n_latent, color_palette)
  }

  # Determine file format and open device
  file_ext <- tools::file_ext(output_file)
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  switch(tolower(file_ext),
         "jpg" = , "jpeg" = jpeg(output_file, width = device_settings$width, height = device_settings$height,
                                 units = device_settings$units, res = device_settings$res),
         "png" = png(output_file, width = device_settings$width, height = device_settings$height,
                     units = device_settings$units, res = device_settings$res),
         "pdf" = pdf(output_file, width = device_settings$width, height = device_settings$height),
         stop("Unsupported file format. Use .jpg, .png, or .pdf")
  )

  # Create plot
  if (show_manifest) {
    # Full model plot with proper node labels and small edge labels
    modified_edge_settings <- edge_settings
    modified_edge_settings$edge.label.cex <- 0.5  # Make edge labels very small

    sem_args <- c(
      list(object = mod, nodeLabels = node_labels, color = color_scheme),
      plot_settings,
      modified_edge_settings
    )
    do.call(semPlot::semPaths, sem_args)
  } else {
    # For latent-only plot, completely hide manifest variables and their loadings
    n_manifest <- nrow(lambda_est)
    n_latent <- ncol(lambda_est)

    # Adjust node labels to only latent variables
    latent_labels <- if (length(node_labels) > n_latent) {
      tail(node_labels, n_latent)  # Take last n_latent labels
    } else {
      node_labels
    }

    # Create modified color scheme - make manifest variables transparent
    modified_color_scheme <- color_scheme
    if (!is.null(modified_color_scheme$man)) {
      # Make manifest variables transparent/white to hide them
      modified_color_scheme$man <- rep("white", length(modified_color_scheme$man))
    }

    # Create plot with only the essential parameters to avoid conflicts
    semPlot::semPaths(
      object = mod,
      nodeLabels = c(rep("", n_manifest), latent_labels), # Empty labels for manifest
      color = modified_color_scheme,
      what = plot_settings$what %||% "std",
      layout = plot_settings$layout %||% "circle",
      curve = plot_settings$curve %||% 0.5,
      curveAdjacent = plot_settings$curveAdjacent %||% TRUE,
      sizeMan = 0,  # Hide manifest variables
      sizeMan2 = 0, # Hide manifest variables
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

  if (save_message) {
    cat("SEM plot saved to:", output_file, "\n")
  }
}
