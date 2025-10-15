compute_netScores <- function(scales = c("EDS", "BSFL", "WHO"),
                                   data = df_completo,
                                   min_loading = 0.20,
                                   rename_dims = FALSE,
                                   custom_names = NULL) {

  # Bibliotecas necesarias
  require(tidyverse)
  require(psych)
  require(qgraph)

  # Detectar automáticamente las columnas de las escalas especificadas
  scale_columns <- list()
  other_columns <- names(data)

  for (scale in scales) {
    # Encontrar columnas que comienzan con el nombre de la escala
    scale_cols <- grep(paste0("^", scale), names(data), value = TRUE)
    if (length(scale_cols) > 0) {
      scale_columns[[scale]] <- scale_cols
      # Remover estas columnas de other_columns
      other_columns <- setdiff(other_columns, scale_cols)
    } else {
      warning(paste("No se encontraron columnas para la escala:", scale))
    }
  }

  # Verificar que se encontraron columnas para procesar
  if (length(scale_columns) == 0) {
    stop("No se encontraron columnas para ninguna de las escalas especificadas")
  }

  cat("\n=== COLUMNAS DETECTADAS ===\n")
  cat("Escalas a procesar:\n")
  for (scale in names(scale_columns)) {
    cat("  ", scale, ": ", length(scale_columns[[scale]]), " columnas\n", sep = "")
  }
  cat("\nOtras variables en el dataset: ", length(other_columns), "\n", sep = "")
  cat("Total de variables: ", ncol(data), "\n\n", sep = "")

  # Función interna para realizar EFA para una escala
  perform_efa <- function(scale_name, data, min_loading) {
    scale_items <- data %>%
      select(starts_with(scale_name)) %>%
      select(-contains("Total"))

    # Determinar número de factores
    n_factors <- case_when(
      scale_name == "EDS" ~ 3,
      scale_name == "BSFL" ~ 1,
      scale_name == "WHO" ~ 1,
      TRUE ~ 1
    )

    # Realizar análisis factorial
    efa_result <- fa(scale_items,
                     nfactors = n_factors,
                     rotate = "oblimin",
                     fm = "ml")

    # Extraer dimensiones
    if (n_factors == 1) {
      loadings_matrix <- as.matrix(efa_result$loadings[, 1, drop = FALSE])
      colnames(loadings_matrix) <- paste0(scale_name, "_Dim1")
    } else {
      loadings_matrix <- efa_result$loadings[, 1:n_factors]
      colnames(loadings_matrix) <- paste0(scale_name, "_Dim", 1:n_factors)
    }

    # Filtrar por min_loading y crear estructura de dimensiones
    dimensions <- list()
    item_loadings <- data.frame()

    for (i in 1:ncol(loadings_matrix)) {
      dim_name <- colnames(loadings_matrix)[i]
      items_in_dim <- names(which(abs(loadings_matrix[, i]) >= min_loading))

      # Ordenar por carga factorial descendente
      loadings_values <- abs(loadings_matrix[items_in_dim, i])
      items_in_dim <- items_in_dim[order(loadings_values, decreasing = TRUE)]

      dimensions[[dim_name]] <- items_in_dim

      # Agregar a item_loadings
      temp_loadings <- data.frame(
        item = items_in_dim,
        loading = loadings_matrix[items_in_dim, i],
        dimension = dim_name,
        stringsAsFactors = FALSE
      )
      item_loadings <- rbind(item_loadings, temp_loadings)
    }

    # Calcular network scores para cada participante
    net_scores <- matrix(NA, nrow = nrow(data), ncol = length(dimensions))
    colnames(net_scores) <- names(dimensions)

    for (i in 1:length(dimensions)) {
      items <- dimensions[[i]]
      if (length(items) > 0) {
        subnet_data <- scale_items[, items]

        # Calcular correlación entre ítems
        cor_matrix <- cor(subnet_data, use = "pairwise.complete.obs")

        # Crear network
        network <- qgraph(cor_matrix,
                          graph = "glasso",
                          sampleSize = nrow(subnet_data),
                          threshold = TRUE,
                          layout = "spring",
                          DoNotPlot = TRUE)

        # Obtener centralidad de cada ítem
        centrality_scores <- centrality(network, alpha = 1, posfun = identity)$InDegree

        # Calcular score ponderado para cada participante
        weights <- centrality_scores / sum(centrality_scores)
        weighted_scores <- as.matrix(subnet_data) %*% weights

        # Estandarizar los scores
        net_scores[, i] <- scale(weighted_scores)[, 1]
      }
    }

    return(list(
      dimensions = dimensions,
      net_scores = net_scores,
      item_loadings = item_loadings
    ))
  }

  # Función para obtener nombres personalizados interactivamente
  get_custom_names <- function(all_dimensions) {
    cat("\n=== DIMENSIONES IDENTIFICADAS ===\n")

    # Mostrar todas las dimensiones
    for (scale in names(all_dimensions)) {
      cat("\n", scale, ":\n", sep = "")
      for (dim_name in names(all_dimensions[[scale]])) {
        items <- all_dimensions[[scale]][[dim_name]]
        n_items <- length(items)
        items_str <- paste(items, collapse = ", ")
        cat("  ", dim_name, " (", n_items, " ítems): ", items_str, "\n", sep = "")
      }
    }

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
          # Si no se proporciona nombre, usar formato NetScore
          num <- gsub(paste0(scale, "_Dim"), "", dim_name)
          custom_names[[dim_name]] <- paste0(scale, "_NetScore_", num)
        } else {
          # Limpiar el nombre ingresado (remover espacios al inicio/final)
          new_name <- trimws(new_name)

          # NO agregar el prefijo de la escala, solo usar lo que escribió el usuario

          # Agregar automáticamente _score al final si no lo tiene
          if (!grepl("_score$", new_name)) {
            new_name <- paste0(new_name, "_score")
          }

          custom_names[[dim_name]] <- new_name

          # Mostrar el nombre final al usuario
          cat("  → Nombre asignado: ", new_name, "\n", sep = "")
        }
      }
    }

    return(custom_names)
  }

  # Procesar cada escala
  all_results <- list()
  all_net_scores <- list()
  all_dimensions <- list()
  all_item_loadings <- list()

  # Actualizar scales para usar solo las que tienen columnas
  scales_to_process <- names(scale_columns)

  for (scale in scales_to_process) {
    cat("Procesando escala:", scale, "\n")
    result <- perform_efa(scale, data, min_loading)
    all_results[[scale]] <- result
    all_net_scores[[scale]] <- result$net_scores
    all_dimensions[[scale]] <- result$dimensions
    all_item_loadings[[scale]] <- result$item_loadings
  }

  # Obtener nombres personalizados si se solicita
  if (rename_dims) {
    if (is.null(custom_names)) {
      # Modo interactivo - MOSTRAR DIMENSIONES Y SOLICITAR NOMBRES
      custom_names <- get_custom_names(all_dimensions)
    } else {
      # Asegurar que los nombres custom también tengan _score al final
      for (name in names(custom_names)) {
        if (!grepl("_score$", custom_names[[name]]) && !grepl("NetScore", custom_names[[name]])) {
          custom_names[[name]] <- paste0(custom_names[[name]], "_score")
        }
      }

      # Mostrar las dimensiones identificadas
      cat("\n=== DIMENSIONES IDENTIFICADAS ===\n")
      for (scale in names(all_dimensions)) {
        cat("\n", scale, ":\n", sep = "")
        for (dim_name in names(all_dimensions[[scale]])) {
          items <- all_dimensions[[scale]][[dim_name]]
          n_items <- length(items)
          items_str <- paste(items, collapse = ", ")
          cat("  ", dim_name, " (", n_items, " ítems): ", items_str, "\n", sep = "")
        }
      }
      cat("\nUsando nombres personalizados proporcionados.\n")
    }
  } else {
    # Usar nombres por defecto (NetScore)
    custom_names <- list()
    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        num <- gsub(paste0(scale, "_Dim"), "", dim_name)
        custom_names[[dim_name]] <- paste0(scale, "_NetScore_", num)
      }
    }
  }

  # Combinar network scores y renombrar columnas
  net_scores_df <- do.call(cbind, all_net_scores)

  # Aplicar nombres personalizados
  new_col_names <- character()
  for (col_name in colnames(net_scores_df)) {
    if (col_name %in% names(custom_names)) {
      new_col_names <- c(new_col_names, custom_names[[col_name]])
    } else {
      new_col_names <- c(new_col_names, col_name)
    }
  }
  colnames(net_scores_df) <- new_col_names
  net_scores_df <- as_tibble(net_scores_df)

  # CALCULAR SUMATORIAS DE ÍTEMS POR DIMENSIÓN
  dimension_sums_df <- data.frame(row.names = 1:nrow(data))

  for (scale in names(all_dimensions)) {
    for (dim_name in names(all_dimensions[[scale]])) {
      items <- all_dimensions[[scale]][[dim_name]]

      if (length(items) > 0) {
        # Obtener nombre personalizado si existe
        if (dim_name %in% names(custom_names)) {
          sum_col_name <- custom_names[[dim_name]]
          # Remover "_score" del nombre para la columna de suma
          sum_col_name <- gsub("_score$", "", sum_col_name)
        } else {
          # Usar nombre por defecto
          sum_col_name <- paste0(scale, "_Sum_", gsub(paste0(scale, "_Dim"), "", dim_name))
        }

        # Calcular suma de los ítems de esta dimensión
        dimension_sums_df[[sum_col_name]] <- rowSums(data[, items], na.rm = TRUE)
      }
    }
  }

  dimension_sums_df <- as_tibble(dimension_sums_df)

  # CREAR DATASET COMPLETO CON SCORES Y SUMAS
  df_complete_with_scores <- bind_cols(data, net_scores_df, dimension_sums_df)

  # Crear resumen de dimensiones con nombres personalizados
  dimension_summary <- character()
  for (scale in names(all_dimensions)) {
    dimension_summary <- c(dimension_summary, paste0("\n", scale, ":"))
    for (dim_name in names(all_dimensions[[scale]])) {
      items <- all_dimensions[[scale]][[dim_name]]
      n_items <- length(items)
      items_str <- paste(items, collapse = ", ")

      # Usar nombre personalizado en el resumen
      display_name <- ifelse(dim_name %in% names(custom_names),
                             custom_names[[dim_name]],
                             dim_name)

      dimension_summary <- c(dimension_summary,
                             paste0("  ", display_name, " (", n_items, "): ", items_str))
    }
  }
  dimension_summary <- paste(dimension_summary, collapse = "\n")

  # Combinar item loadings
  item_loadings_df <- do.call(rbind, all_item_loadings) %>%
    as_tibble() %>%
    mutate(loading = round(as.numeric(loading), 3)) %>%
    arrange(dimension, desc(abs(loading))) %>%
    select(item, loading)

  # Crear resumen de la data final
  data_summary <- paste0(
    "\n=== RESUMEN DEL DATASET FINAL ===\n",
    "Filas: ", nrow(df_complete_with_scores), "\n",
    "Columnas totales: ", ncol(df_complete_with_scores), "\n",
    "  - Variables originales: ", ncol(data), "\n",
    "  - Network scores agregados: ", ncol(net_scores_df), "\n",
    "  - Sumatorias de dimensiones: ", ncol(dimension_sums_df), "\n",
    "  - Variables de escalas procesadas: ", sum(sapply(scale_columns, length)), "\n",
    "  - Otras variables: ", length(other_columns), "\n"
  )

  # Retornar resultados
  resultado <- list(
    # Dataset completo con scores y sumas
    data_complete = df_complete_with_scores,

    # Solo los network scores
    net_scores = net_scores_df,

    # Solo las sumatorias
    dimension_sums = dimension_sums_df,

    # Resúmenes y detalles
    dimension_summary = dimension_summary,
    item_loadings = item_loadings_df,
    dimensions_list = all_dimensions,
    dimension_names = custom_names,

    # Información adicional
    scale_columns = scale_columns,
    other_columns = other_columns,
    data_summary = data_summary
  )

  class(resultado) <- c("scale_network_analysis", "list")

  return(resultado)
}
