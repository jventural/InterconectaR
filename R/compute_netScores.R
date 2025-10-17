compute_netScores <- function(item_prefixes,
                                     data,
                                     rename_dims = TRUE,
                                     custom_names = NULL,
                                     add_sums = TRUE,
                                     # Parámetros para análisis de estabilidad
                                     stability_threshold = 0.70,
                                     stability_corr = "spearman",
                                     stability_model = "glasso",
                                     stability_algorithm = "louvain",
                                     stability_iter = 1000,
                                     stability_seed_start = 2025,
                                     stability_type = "resampling",
                                     stability_ncores = 11,
                                     stability_max_iter = 10,
                                     stability_plot = TRUE,
                                     # Parámetros para EGA
                                     ega_plot = TRUE) {

  require(tidyverse)
  require(EGAnet)
  require(InterconectaR)

  cat("\n=== COMPUTE NETWORK SCORES CON ANÁLISIS DE ESTABILIDAD ===\n\n")

  # Validar entrada
  if (!is.character(item_prefixes) || length(item_prefixes) == 0) {
    stop("item_prefixes debe ser un vector de caracteres con los prefijos de items (e.g., c('BP', 'SC', 'EC'))")
  }

  all_dimensions <- list()
  all_net_scores <- list()
  scale_columns <- list()
  ega_objects <- list()
  stability_results <- list()
  removed_items_by_scale <- list()

  # Procesar cada escala
  for (prefix in item_prefixes) {

    cat("\n===============================================\n")
    cat("PROCESANDO ESCALA:", prefix, "\n")
    cat("===============================================\n\n")

    # Extraer datos con el prefijo
    scale_data_original <- data %>% select(starts_with(prefix))

    if (ncol(scale_data_original) == 0) {
      warning(paste0("No se encontraron items con el prefijo '", prefix, "'. Saltando..."))
      next
    }

    cat("Items originales (", ncol(scale_data_original), "):", paste(names(scale_data_original), collapse = ", "), "\n\n")

    # ============================================================
    # PASO 1: ANÁLISIS DE ESTABILIDAD
    # ============================================================
    cat("--- PASO 1: ANÁLISIS DE ESTABILIDAD ---\n")

    tryCatch({
      model_refinado <- refine_items_by_stability(
        data = scale_data_original,
        threshold = stability_threshold,
        corr = stability_corr,
        model = stability_model,
        algorithm = stability_algorithm,
        iter = stability_iter,
        seed_start = stability_seed_start,
        type = stability_type,
        ncores = stability_ncores,
        max_iter = stability_max_iter,
        plot.itemStability = stability_plot
      )

      # Guardar resultados de estabilidad
      stability_results[[prefix]] <- model_refinado

      # Obtener items removidos
      items_removidos <- unique(c(model_refinado$removed_items))
      removed_items_by_scale[[prefix]] <- items_removidos

      if (length(items_removidos) > 0) {
        cat("\nItems REMOVIDOS por inestabilidad (", length(items_removidos), "):\n")
        cat("  ", paste(items_removidos, collapse = ", "), "\n\n")
      } else {
        cat("\nNo se removieron items (todos son estables).\n\n")
      }

      # Datos refinados (sin items inestables)
      scale_data_refinada <- scale_data_original %>%
        select(-any_of(items_removidos))

      cat("Items FINALES después de refinamiento (", ncol(scale_data_refinada), "):",
          paste(names(scale_data_refinada), collapse = ", "), "\n\n")

    }, error = function(e) {
      cat("\nERROR en análisis de estabilidad:", conditionMessage(e), "\n")
      cat("Usando datos originales sin refinamiento.\n\n")
      scale_data_refinada <<- scale_data_original
      removed_items_by_scale[[prefix]] <<- character(0)
    })

    # ============================================================
    # PASO 2: ESTIMACIÓN DE EGA
    # ============================================================
    cat("--- PASO 2: ESTIMACIÓN DE EGA ---\n")

    ega_obj <- EGA(
      data = scale_data_refinada,
      plot.EGA = ega_plot
    )

    ega_objects[[prefix]] <- ega_obj

    # Obtener membresía de ítems (comunidades)
    memberships <- ega_obj$wc
    n_dims <- max(memberships)

    cat("Dimensiones identificadas:", n_dims, "\n\n")

    # Obtener nombres de ítems finales
    item_names <- names(memberships)
    scale_columns[[prefix]] <- item_names

    # Organizar ítems por dimensión
    scale_dims <- list()
    for (dim_num in 1:n_dims) {
      dim_name <- paste0(prefix, "_Dim", dim_num)
      items_in_dim <- item_names[memberships == dim_num]
      scale_dims[[dim_name]] <- items_in_dim
    }

    all_dimensions[[prefix]] <- scale_dims

    # ============================================================
    # PASO 3: CALCULAR NETWORK SCORES
    # ============================================================
    cat("--- PASO 3: CALCULANDO NETWORK SCORES ---\n")

    # Calcular net.scores
    net_scores_result <- net.scores(data = scale_data_refinada, A = ega_obj)

    # Extraer scores estandarizados
    std_scores_matrix <- net_scores_result$scores$std.scores

    # Convertir a data frame y nombrar columnas
    net_scores_df <- as.data.frame(std_scores_matrix)
    colnames(net_scores_df) <- names(scale_dims)

    all_net_scores[[prefix]] <- net_scores_df

    cat("Network scores calculados exitosamente.\n")
  }

  cat("\n\n=== RESUMEN: DIMENSIONES IDENTIFICADAS ===\n")
  for (scale in names(all_dimensions)) {
    cat("\n", scale, ":\n", sep = "")
    for (dim_name in names(all_dimensions[[scale]])) {
      items <- all_dimensions[[scale]][[dim_name]]
      n_items <- length(items)
      items_str <- paste(items, collapse = ", ")
      cat("  ", dim_name, " (", n_items, " ítems): ", items_str, "\n", sep = "")
    }
  }

  # Decidir si renombrar
  if (is.null(custom_names) && rename_dims) {
    cat("\n=== RENOMBRAR DIMENSIONES ===\n")
    cat("Proporciona un nombre descriptivo para cada dimensión:\n")
    cat("(Presiona Enter para mantener el nombre por defecto)\n")
    cat("Nota: Se agregará automáticamente '_score' al final del nombre\n\n")

    custom_names <- list()

    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        items <- all_dimensions[[scale]][[dim_name]]
        cat("\n", dim_name, " (ítems: ", paste(items, collapse = ", "), ")\n", sep = "")
        new_name <- readline(prompt = paste0("Nuevo nombre para ", dim_name, ": "))

        if (new_name == "") {
          # Mantener nombre por defecto (NetScore)
          num <- gsub(paste0(scale, "_Dim"), "", dim_name)
          custom_names[[dim_name]] <- paste0(scale, "_NetScore_", num)
        } else {
          new_name <- trimws(new_name)
          # Agregar _score si no lo tiene
          if (!grepl("_score$", new_name)) {
            new_name <- paste0(new_name, "_score")
          }
          custom_names[[dim_name]] <- new_name
          cat("  → Nombre asignado:", new_name, "\n")
        }
      }
    }
  } else if (!rename_dims || is.null(custom_names)) {
    # Usar nombres por defecto
    custom_names <- list()
    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        num <- gsub(paste0(scale, "_Dim"), "", dim_name)
        custom_names[[dim_name]] <- paste0(scale, "_NetScore_", num)
      }
    }
    cat("\nUsando nombres por defecto.\n")
  }

  # Combinar network scores
  net_scores_combined <- bind_cols(all_net_scores)

  # Aplicar nombres personalizados
  new_col_names <- character()
  for (col_name in colnames(net_scores_combined)) {
    if (col_name %in% names(custom_names)) {
      new_col_names <- c(new_col_names, custom_names[[col_name]])
    } else {
      new_col_names <- c(new_col_names, col_name)
    }
  }
  colnames(net_scores_combined) <- new_col_names

  # Calcular sumatorias si se solicita
  dimension_sums <- NULL
  if (add_sums) {
    cat("\n=== CALCULANDO SUMATORIAS ===\n")
    dimension_sums_df <- data.frame(row.names = 1:nrow(data))

    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        items <- all_dimensions[[scale]][[dim_name]]

        if (length(items) > 0) {
          # Obtener nombre personalizado
          if (dim_name %in% names(custom_names)) {
            sum_col_name <- gsub("_score$", "_sum", custom_names[[dim_name]])
            if (!grepl("_sum$", sum_col_name)) {
              sum_col_name <- paste0(sum_col_name, "_sum")
            }
          } else {
            sum_col_name <- paste0(scale, "_Sum_", gsub(paste0(scale, "_Dim"), "", dim_name))
          }

          dimension_sums_df[[sum_col_name]] <- rowSums(data[, items, drop = FALSE], na.rm = TRUE)
        }
      }
    }

    dimension_sums <- as_tibble(dimension_sums_df)
  }

  # Dataset completo
  if (!is.null(dimension_sums)) {
    df_complete <- bind_cols(data, net_scores_combined, dimension_sums)
  } else {
    df_complete <- bind_cols(data, net_scores_combined)
  }

  cat("\n=== RESUMEN FINAL ===\n")
  cat("Total de escalas procesadas:", length(all_dimensions), "\n")
  cat("Total de dimensiones:", ncol(net_scores_combined), "\n")
  cat("Network scores:", paste(names(net_scores_combined), collapse = ", "), "\n")

  if (add_sums) {
    cat("Sumatorias:", ncol(dimension_sums), "\n")
  }

  cat("Dataset final:", ncol(df_complete), "columnas\n\n")

  # Retornar resultado
  resultado <- list(
    data_complete = df_complete,
    net_scores = as_tibble(net_scores_combined),
    dimension_sums = dimension_sums,
    dimensions_list = all_dimensions,
    dimension_names = custom_names,
    ega_objects = ega_objects,
    scale_columns = scale_columns,
    stability_results = stability_results,
    removed_items = removed_items_by_scale
  )

  class(resultado) <- c("ega_network_scores", "list")

  return(resultado)
}
