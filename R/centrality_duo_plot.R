centrality_duo_plot <- function(qgraph_obj,
                                network,
                                groups = NULL,
                                measure0 = "ExpectedInfluence",
                                measure1 = NULL,
                                color_palette = c("#6C5CE7", "#00B894"),
                                use_abbrev = TRUE,
                                labels = NULL) {  # NUEVO PARÁMETRO
  # Cargar librerías necesarias
  library(qgraph); library(dplyr); library(networktools)
  library(tibble); library(ggplot2); library(tidyr)
  library(stringr); library(forcats); library(purrr)

  # Etiquetas para las métricas
  label0 <- measure0
  label1 <- measure1

  # --- Calcular centralidad principal ---
  ctbl <- centralityTable(network)

  # Obtener nombres completos
  full_names <- rownames(network$graph)
  if (is.null(full_names)) full_names <- colnames(network$graph)

  # NUEVO: Determinar qué etiquetas usar
  if (!is.null(labels)) {
    # Validar longitud de labels personalizados
    if (length(labels) != length(full_names)) {
      stop(paste("La longitud de 'labels' (", length(labels),
                 ") debe coincidir con el número de nodos (",
                 length(full_names), ")"))
    }
    qgraph_labels <- labels
  } else {
    # Usar etiquetas del qgraph
    qgraph_labels <- qgraph_obj$graphAttributes$Nodes$labels
  }

  # Crear mapeo de nombres completos a abreviaciones
  name_to_abbrev <- setNames(qgraph_labels, full_names)

  cents_expect <- ctbl %>%
    filter(measure == !!measure0) %>%
    select(full_name = node, value) %>%
    arrange(full_name) %>%
    rename(!!measure0 := value) %>%
    mutate(
      Abrev = name_to_abbrev[full_name]
    )

  # --- Calcular medidas de puente (si se solicitan) ---
  if (!is.null(measure1)) {

    # Validar que existan grupos
    if (is.null(groups)) {
      warning("No se pueden calcular medidas de puente sin definir grupos. Se omitirá measure1.")
      measure1 <- NULL
      label1 <- NULL
    } else {

      tryCatch({
        # Calcular medidas de puente
        b_obj <- bridge(qgraph_obj,
                        communities    = groups,
                        useCommunities = "all",
                        normalize      = FALSE)

        # Validar resultado de bridge
        if (is.null(b_obj)) {
          warning("La función bridge() devolvió NULL. Se omitirá measure1.")
          measure1 <- NULL
          label1 <- NULL
        } else if (!measure1 %in% names(b_obj)) {
          warning(paste("La medida", measure1, "no se encontró en los resultados de bridge. Medidas disponibles:",
                        paste(names(b_obj), collapse = ", "), ". Se omitirá measure1."))
          measure1 <- NULL
          label1 <- NULL
        } else {

          bridge_values <- b_obj[[measure1]]

          # Validar valores de puente
          if (is.null(bridge_values) || length(bridge_values) == 0) {
            warning(paste("La medida", measure1, "está vacía o es NULL. Se omitirá measure1."))
            measure1 <- NULL
            label1 <- NULL
          } else {

            # MODIFICADO: Usar las etiquetas determinadas anteriormente
            # Validar longitud
            if (length(bridge_values) != length(qgraph_labels)) {
              warning(paste("Longitud de bridge_values (", length(bridge_values),
                            ") no coincide con qgraph_labels (", length(qgraph_labels),
                            "). Se omitirá measure1."))
              measure1 <- NULL
              label1 <- NULL
            } else {

              # Crear tabla de bridge estandarizada
              bridge_data <- tibble(
                qgraph_label = qgraph_labels,
                raw_bridge   = as.numeric(bridge_values)
              ) %>%
                mutate(!!measure1 := as.numeric(scale(raw_bridge))) %>%
                select(-raw_bridge)

              # Mapear nombres completos
              name_mapping <- tibble(
                full_name    = full_names,
                qgraph_label = qgraph_labels
              )

              # Unir con abreviaciones
              bridge_data <- bridge_data %>%
                left_join(name_mapping, by = "qgraph_label") %>%
                mutate(
                  Abrev = name_to_abbrev[full_name]
                )

              # Combinar centralidad y puente
              cents2 <- inner_join(
                cents_expect,
                bridge_data %>% select(full_name, !!sym(measure1)),
                by = "full_name"
              )

              # Verificar join exitoso
              if (nrow(cents2) == 0) {
                if (nrow(cents_expect) == nrow(bridge_data)) {
                  cents2 <- bind_cols(
                    cents_expect,
                    bridge_data %>% select(!!sym(measure1))
                  )
                } else {
                  warning("No se pueden unir centralidad y puente por diferencia en número de filas. Se omitirá measure1.")
                  measure1 <- NULL
                  label1 <- NULL
                }
              }
            }
          }
        }
      }, error = function(e) {
        warning(paste("Error al calcular medidas de puente:", e$message, ". Se omitirá measure1."))
        measure1 <- NULL
        label1 <- NULL
      })
    }
  }

  # --- Generar gráfico con dos medidas ---
  if (!is.null(measure1) && !is.null(label1) && exists("cents2")) {
    y_var     <- if (use_abbrev) quo(Abrev) else quo(full_name)

    # Transformar a formato largo para ggplot
    cents_long <- cents2 %>%
      pivot_longer(
        cols      = c(!!sym(measure0), !!sym(measure1)),
        names_to  = "Measure",
        values_to = "Value"
      )

    # Configurar paleta de colores
    pal    <- setNames(color_palette, c(label0, label1))
    breaks <- c(label0, label1)
    labels <- c(label0, label1)

    # Crear gráfico
    Figura <- ggplot(cents_long,
                     aes(x = Value,
                         y = fct_reorder(!!y_var, Value),
                         color = Measure, group = Measure
                     )) +
      geom_point(size = 4, alpha = 0.8) +
      geom_line(linewidth = 1.2, alpha = 0.7) +
      theme_minimal() +
      labs(x = "z-score", y = "Nodos", color = "Métrica") +
      scale_color_manual(
        values = pal,
        breaks = breaks,
        labels = labels
      ) +
      theme(
        axis.text.y  = element_text(size = 12, face = "bold"),
        axis.text.x  = element_text(size = 12),
        axis.title   = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey95", size = 0.3),
        plot.background = element_rect(fill = "white", color = NA)
      )

    # Suprimir warnings al imprimir
    class(Figura) <- c("silent_gg", class(Figura))
    assign("print.silent_gg",
           function(x, ...) suppressWarnings(NextMethod()),
           envir = .GlobalEnv)

    return(list(
      table = cents2 %>% arrange(desc(!!sym(measure0))),
      plot  = Figura
    ))

    # --- Generar gráfico con una sola medida ---
  } else {
    cents_single <- cents_expect %>%
      select(full_name, Abrev, !!sym(measure0)) %>%
      arrange(desc(!!sym(measure0))) %>%
      rename(Value = !!sym(measure0))

    y_var <- if (use_abbrev) quo(Abrev) else quo(full_name)
    pal_single <- setNames(color_palette[1], label0)

    Figura <- ggplot(cents_single,
                     aes(x = Value,
                         y = fct_reorder(!!y_var, Value),
                         group = 1)) +
      geom_line(color = pal_single, size = 1.2, alpha = 0.7) +
      geom_point(color = pal_single, size = 4, alpha = 0.8) +
      theme_minimal() +
      labs(x = "z-score", y = "Nodos", color = "Métrica") +
      scale_color_manual(
        values = pal_single,
        breaks = label0,
        labels = label0
      ) +
      theme(
        axis.text.y  = element_text(size = 12, face = "bold"),
        axis.text.x  = element_text(size = 12),
        axis.title   = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey95", size = 0.3),
        plot.background = element_rect(fill = "white", color = NA)
      )

    # Suprimir warnings al imprimir
    class(Figura) <- c("silent_gg", class(Figura))
    assign("print.silent_gg",
           function(x, ...) suppressWarnings(NextMethod()),
           envir = .GlobalEnv)

    return(list(table = cents_single, plot = Figura))
  }
}
