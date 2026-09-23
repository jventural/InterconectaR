#' Compute Network Scores
#'
#' Computes network scores with stability analysis using EGA.
#'
#' @param item_prefixes Character vector of item prefix patterns.
#' @param data Data frame with item responses.
#' @param rename_dims Logical; ask for a name for each dimension. The prompt
#'   only appears in interactive sessions; otherwise, or when `FALSE`, the
#'   default names are used.
#' @param custom_names Optional named list of dimension names.
#' @param add_sums Logical; compute sum scores per dimension.
#' @param stability_threshold Minimum item stability threshold.
#' @param stability_corr Correlation method for stability analysis.
#' @param stability_model Network model for stability analysis.
#' @param stability_algorithm Community detection algorithm.
#' @param stability_iter Number of bootstrap iterations.
#' @param stability_seed_start Optional starting random seed (see
#'   [refine_items_by_stability()]). `NULL` (default) leaves the seed to
#'   `EGAnet::bootEGA()`.
#' @param stability_type Bootstrap type.
#' @param stability_ncores Number of CPU cores (default 2).
#' @param stability_max_iter Maximum refinement iterations.
#' @param stability_plot Logical; plot item stability.
#' @param ega_plot Logical; plot EGA results.
#' @param verbose Logical; report the progress of each step as messages.
#'
#' @return A list of class `ega_network_scores` with `data_complete` (the
#'   data with the network scores and sums appended), `net_scores`,
#'   `dimension_sums`, `dimensions_list`, `dimension_names`, `ega_objects`,
#'   `scale_columns`, `stability_results` and `removed_items`.
#' @export
#' @importFrom dplyr %>% select starts_with bind_cols any_of
#' @importFrom tibble as_tibble
#' @importFrom EGAnet EGA net.scores
#' @importFrom rlang sym !! :=
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' f1 <- rnorm(n)
#' f2 <- 0.3 * f1 + rnorm(n)
#' items <- data.frame(
#'   sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'   sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#' )
#' names(items) <- c(paste0("SC", 1:4), paste0("SC", 5:8))
#'
#' scores <- compute_netScores("SC", items, rename_dims = FALSE,
#'                             stability_corr = "pearson",
#'                             stability_iter = 20, stability_seed_start = 1,
#'                             stability_ncores = 1, stability_plot = FALSE,
#'                             ega_plot = FALSE)
#' head(scores$net_scores)
#' }
compute_netScores <- function(item_prefixes,
                                     data,
                                     rename_dims = TRUE,
                                     custom_names = NULL,
                                     add_sums = TRUE,
                                     # Parametros para analisis de estabilidad
                                     stability_threshold = 0.70,
                                     stability_corr = "spearman",
                                     stability_model = "glasso",
                                     stability_algorithm = "louvain",
                                     stability_iter = 1000,
                                     stability_seed_start = NULL,
                                     stability_type = "resampling",
                                     stability_ncores = 2,
                                     stability_max_iter = 10,
                                     stability_plot = TRUE,
                                     # Parametros para EGA
                                     ega_plot = TRUE,
                                     verbose = TRUE) {

  say <- function(...) if (isTRUE(verbose)) message(...)

  say("=== COMPUTE NETWORK SCORES CON AN\u00c1LISIS DE ESTABILIDAD ===")

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

    say("PROCESANDO ESCALA: ", prefix)

    # Extraer datos con el prefijo
    scale_data_original <- data %>% select(starts_with(prefix))

    if (ncol(scale_data_original) == 0) {
      warning(paste0("No se encontraron items con el prefijo '", prefix, "'. Saltando..."))
      next
    }

    say("Items originales (", ncol(scale_data_original), "): ",
        paste(names(scale_data_original), collapse = ", "))

    # ============================================================
    # PASO 1: ANALISIS DE ESTABILIDAD
    # ============================================================
    say("--- PASO 1: AN\u00c1LISIS DE ESTABILIDAD ---")

    model_refinado <- tryCatch(
      refine_items_by_stability(
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
      ),
      error = function(e) e
    )

    if (inherits(model_refinado, "error")) {
      # Sin refinamiento: se usan los datos originales
      say("ERROR en an\u00e1lisis de estabilidad: ", conditionMessage(model_refinado),
          ". Usando datos originales sin refinamiento.")
      scale_data_refinada <- scale_data_original
      removed_items_by_scale[[prefix]] <- character(0)
    } else {
      # Guardar resultados de estabilidad
      stability_results[[prefix]] <- model_refinado

      # Obtener items removidos
      items_removidos <- unique(c(model_refinado$removed_items))
      removed_items_by_scale[[prefix]] <- items_removidos

      if (length(items_removidos) > 0) {
        say("Items REMOVIDOS por inestabilidad (", length(items_removidos), "): ",
            paste(items_removidos, collapse = ", "))
      } else {
        say("No se removieron items (todos son estables).")
      }

      # Datos refinados (sin items inestables)
      scale_data_refinada <- scale_data_original %>%
        select(-any_of(items_removidos))

      say("Items FINALES despu\u00e9s de refinamiento (", ncol(scale_data_refinada), "): ",
          paste(names(scale_data_refinada), collapse = ", "))
    }

    # ============================================================
    # PASO 2: ESTIMACION DE EGA
    # ============================================================
    say("--- PASO 2: ESTIMACI\u00d3N DE EGA ---")

    ega_obj <- EGA(
      data = scale_data_refinada,
      plot.EGA = ega_plot
    )

    ega_objects[[prefix]] <- ega_obj

    # Obtener membresia de items (comunidades)
    memberships <- ega_obj$wc
    n_dims <- max(memberships, na.rm = TRUE)

    say("Dimensiones identificadas: ", n_dims)

    # Obtener nombres de items finales
    item_names <- names(memberships)
    scale_columns[[prefix]] <- item_names

    # Organizar items por dimension
    scale_dims <- list()
    for (dim_num in seq_len(n_dims)) {
      dim_name <- paste0(prefix, "_Dim", dim_num)
      items_in_dim <- item_names[!is.na(memberships) & memberships == dim_num]
      scale_dims[[dim_name]] <- items_in_dim
    }

    all_dimensions[[prefix]] <- scale_dims

    # ============================================================
    # PASO 3: CALCULAR NETWORK SCORES
    # ============================================================
    say("--- PASO 3: CALCULANDO NETWORK SCORES ---")

    # Calcular net.scores
    net_scores_result <- net.scores(data = scale_data_refinada, A = ega_obj)

    # Extraer scores estandarizados
    std_scores_matrix <- net_scores_result$scores$std.scores

    # Convertir a data frame y nombrar columnas
    net_scores_df <- as.data.frame(std_scores_matrix)
    colnames(net_scores_df) <- names(scale_dims)

    all_net_scores[[prefix]] <- net_scores_df

    say("Network scores calculados exitosamente.")
  }

  if (isTRUE(verbose)) {
    resumen <- character(0)
    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        items <- all_dimensions[[scale]][[dim_name]]
        resumen <- c(resumen, paste0("  ", dim_name, " (", length(items), " \u00edtems): ",
                                     paste(items, collapse = ", ")))
      }
    }
    message("=== RESUMEN: DIMENSIONES IDENTIFICADAS ===\n", paste(resumen, collapse = "\n"))
  }

  # Decidir si renombrar (la pregunta solo tiene sentido en una sesion interactiva)
  if (is.null(custom_names) && rename_dims && interactive()) {
    message("=== RENOMBRAR DIMENSIONES ===\n",
            "Proporciona un nombre descriptivo para cada dimensi\u00f3n ",
            "(Enter mantiene el nombre por defecto; se agrega '_score' al final).")

    custom_names <- list()

    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        items <- all_dimensions[[scale]][[dim_name]]
        message(dim_name, " (\u00edtems: ", paste(items, collapse = ", "), ")")
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
          message("  -> Nombre asignado: ", new_name)
        }
      }
    }
  } else if (is.null(custom_names)) {
    # Usar nombres por defecto
    custom_names <- list()
    for (scale in names(all_dimensions)) {
      for (dim_name in names(all_dimensions[[scale]])) {
        num <- gsub(paste0(scale, "_Dim"), "", dim_name)
        custom_names[[dim_name]] <- paste0(scale, "_NetScore_", num)
      }
    }
    say("Usando nombres por defecto.")
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
    say("=== CALCULANDO SUMATORIAS ===")
    dimension_sums_df <- data.frame(row.names = seq_len(nrow(data)))

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

  say("=== RESUMEN FINAL ===\n",
      "Total de escalas procesadas: ", length(all_dimensions), "\n",
      "Total de dimensiones: ", ncol(net_scores_combined), "\n",
      "Network scores: ", paste(names(net_scores_combined), collapse = ", "), "\n",
      if (add_sums) paste0("Sumatorias: ", ncol(dimension_sums), "\n") else "",
      "Dataset final: ", ncol(df_complete), " columnas")

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
