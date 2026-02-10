#' Estimate Networks by Group
#'
#' Estimates network models separately for each group in the data.
#'
#' @param data Data frame containing the data.
#' @param group_var Name of the grouping variable column.
#' @param columns Character vector of variable names to include.
#' @param default Network estimation method (default: "ggmModSelect").
#' @param stepwise Logical; use stepwise model selection.
#' @param corMethod Correlation method (default: "spearman").
#' @param abbreviate_vars Logical; abbreviate variable names.
#' @param abbr_minlength Minimum length for abbreviated names.
#' @param labels Optional custom labels for nodes.
#' @param set_labels_on_graph Logical; apply labels to graph object.
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom bootnet estimateNetwork
#' @importFrom purrr map
#' @importFrom tibble tibble
estimate_networks_by_group <- function(data,
                                       group_var,
                                       columns,
                                       default          = "ggmModSelect",
                                       stepwise         = TRUE,
                                       corMethod        = "spearman",
                                       abbreviate_vars  = FALSE,
                                       abbr_minlength   = 3,
                                       labels           = NULL,   # << NUEVO
                                       set_labels_on_graph = TRUE # << NUEVO
) {
  # -------- Helpers --------
  # Construye las etiquetas finales por variable (original -> label)
  build_label_map <- function(orig_names, labels, abbreviate_vars, abbr_minlength) {
    if (!is.null(labels)) {
      # Caso A: labels NOMBRADO: c("BP1"="Item 1", "BP2"="Item 2", ...)
      if (!is.null(names(labels)) && any(nzchar(names(labels)))) {
        lab_map <- labels[orig_names]
        if (any(is.na(lab_map))) {
          missing <- paste(orig_names[is.na(lab_map)], collapse = ", ")
          stop("Faltan etiquetas para: ", missing)
        }
        names(lab_map) <- orig_names
        return(lab_map)
      }
      # Caso B: labels NO NOMBRADO: debe coincidir en longitud y orden con 'columns'
      if (length(labels) != length(orig_names)) {
        stop("Si 'labels' no es nombrado, su longitud debe ser igual a length(columns).")
      }
      names(labels) <- orig_names
      return(labels)
    } else {
      # Sin labels personalizados: usar abreviaciones o nombres originales
      if (abbreviate_vars) {
        ab <- abbreviate(orig_names, minlength = abbr_minlength)
        names(ab) <- orig_names
        return(ab)
      } else {
        nm <- orig_names
        names(nm) <- orig_names
        return(nm)
      }
    }
  }

  # -------- Preparacion por grupos --------
  unique_groups <- unique(data[[group_var]])

  # Dividir el data.frame por grupo y quitar la columna de grupo
  # Usar subset en lugar de select para evitar conflicto con BGGM::select
  data_subset <- data[, c(columns, group_var), drop = FALSE]

  list_of_data <- list()
  for (grp in unique_groups) {
    grp_data <- data_subset[data_subset[[group_var]] == grp, columns, drop = FALSE]
    list_of_data[[as.character(grp)]] <- grp_data
  }

  # Etiquetas finales (mismas para todos los grupos porque columnas son las mismas)
  orig_names_global <- columns
  label_map_global  <- build_label_map(orig_names_global, labels, abbreviate_vars, abbr_minlength)
  # label_map_global es un named vector: names = originales, values = labels finales

  # -------- Estimacion por grupo --------
  estimate_for_group <- function(df_group) {
    # Asegurar el mismo orden de columnas que 'columns'
    df_group <- df_group[, orig_names_global, drop = FALSE]

    # (Mantengo colnames del df para estabilidad; labels se aplican al objeto estimado)
    # Si antes abreviabas colnames(df_group), ya no es necesario: las etiquetas van por dimnames

    # Estimar la red - solo pasar stepwise si el metodo lo soporta
    if (default == "ggmModSelect") {
      net <- bootnet::estimateNetwork(
        df_group,
        default   = default,
        stepwise  = stepwise,
        corMethod = corMethod
      )
    } else {
      net <- bootnet::estimateNetwork(
        df_group,
        default   = default,
        corMethod = corMethod
      )
    }

    # Aplicar labels al objeto (dimnames del grafo y, si existe, datos internos)
    if (isTRUE(set_labels_on_graph)) {
      final_labels <- unname(label_map_global[colnames(df_group)])  # en orden
      if (!is.null(net$graph)) {
        dimnames(net$graph) <- list(final_labels, final_labels)
      }
      if (!is.null(net$data)) {
        colnames(net$data) <- final_labels
      }
    }

    # Guardar trazabilidad
    attr(net, "var_mapping") <- tibble::tibble(
      original = orig_names_global,
      label    = unname(label_map_global[orig_names_global])
    )

    return(net)
  }

  list_of_networks <- purrr::map(list_of_data, estimate_for_group)

  return(list_of_networks)
}
