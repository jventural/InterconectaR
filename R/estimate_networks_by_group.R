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
  # Instalar/cargar paquetes necesarios
  if (!requireNamespace("dplyr", quietly = TRUE))     install.packages("dplyr")
  if (!requireNamespace("bootnet", quietly = TRUE))   install.packages("bootnet")
  if (!requireNamespace("purrr", quietly = TRUE))     install.packages("purrr")

  library(dplyr)
  library(bootnet)
  library(purrr)

  # -------- Helpers --------
  # Construye las etiquetas finales por variable (original -> label)
  build_label_map <- function(orig_names, labels, abbreviate_vars, abbr_minlength) {
    if (!is.null(labels)) {
      # Caso A: labels NOMBRADO: c("BP1"="Ítem 1", "BP2"="Ítem 2", ...)
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

  # -------- Preparación por grupos --------
  unique_groups <- unique(data[[group_var]])

  # Dividir el data.frame por grupo y quitar la columna de grupo
  list_of_data <- data %>%
    dplyr::filter(.data[[group_var]] %in% unique_groups) %>%
    dplyr::select(dplyr::all_of(columns), dplyr::all_of(group_var)) %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::group_split() %>%
    purrr::map(~ dplyr::select(.x, -dplyr::all_of(group_var))) %>%
    purrr::set_names(unique_groups)

  # Etiquetas finales (mismas para todos los grupos porque columnas son las mismas)
  orig_names_global <- columns
  label_map_global  <- build_label_map(orig_names_global, labels, abbreviate_vars, abbr_minlength)
  # label_map_global es un named vector: names = originales, values = labels finales

  # -------- Estimación por grupo --------
  estimate_for_group <- function(df_group) {
    # Asegurar el mismo orden de columnas que 'columns'
    df_group <- df_group[, orig_names_global, drop = FALSE]

    # (Mantengo colnames del df para estabilidad; labels se aplican al objeto estimado)
    # Si antes abreviabas colnames(df_group), ya no es necesario: las etiquetas van por dimnames

    # Estimar la red
    net <- bootnet::estimateNetwork(
      df_group,
      default   = default,
      stepwise  = stepwise,
      corMethod = corMethod
    )

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
