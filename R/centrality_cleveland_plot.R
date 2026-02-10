#' Cleveland Dot Plot para Indices de Centralidad
#'
#' Crea un grafico tipo Cleveland Dot Plot para visualizar dos metricas de centralidad
#' (por ejemplo, Strength y Bridge Strength). Estilo clasico y elegante para publicaciones.
#'
#' @param qgraph_obj Objeto qgraph creado previamente.
#' @param network Objeto de red estimado (por ejemplo, de bootnet::estimateNetwork).
#' @param groups Lista de grupos/comunidades para calcular metricas de puente.
#' @param measure0 Metrica principal. Default: "Strength".
#' @param measure1 Metrica secundaria. Default: "Bridge Strength".
#' @param color0 Color para measure0. Default: "#2C3E50".
#' @param color1 Color para measure1. Default: "#E74C3C".
#' @param point_size Tamano de los puntos. Default: 4.
#' @param segment_size Grosor de las lineas de conexion. Default: 0.8.
#' @param segment_color Color de la linea que conecta los puntos. Default: "grey60".
#' @param use_abbrev Usar nombres abreviados. Default: TRUE.
#' @param labels Vector de etiquetas personalizadas para los nodos. Default: NULL.
#' @param order_by Ordenar por: "measure0", "measure1", "difference", "name". Default: "measure0".
#' @param show_zero_line Mostrar linea vertical en cero. Default: TRUE.
#' @param text_size Tamano base del texto. Default: 12.
#'
#' @return Lista con dos elementos:
#'   \item{plot}{Objeto ggplot con el Cleveland Dot Plot.}
#'   \item{table}{Tabla con los valores de centralidad.}
#'
#' @examples
#' \dontrun{
#' result <- centrality_cleveland_plot(
#'   qgraph_obj = g1,
#'   network = network,
#'   groups = groups,
#'   measure0 = "Strength",
#'   measure1 = "Bridge Strength"
#' )
#' result$plot
#' result$table
#' }
#'
#' @export
#' @importFrom qgraph centralityTable
#' @importFrom dplyr %>% filter select arrange rename mutate left_join desc any_of
#' @importFrom networktools bridge
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_vline geom_segment geom_point scale_color_manual scale_shape_manual guides guide_legend labs theme_minimal theme element_text element_blank element_rect element_line margin
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom rlang sym !! :=
centrality_cleveland_plot <- function(qgraph_obj,
                                      network,
                                      groups = NULL,
                                      measure0 = "Strength",
                                      measure1 = "Bridge Strength",
                                      color0 = "#2C3E50",
                                      color1 = "#E74C3C",
                                      point_size = 4,
                                      segment_size = 0.8,
                                      segment_color = "grey60",
                                      use_abbrev = TRUE,
                                      labels = NULL,
                                      order_by = "measure0",
                                      show_zero_line = TRUE,
                                      text_size = 12) {

  # Obtener nombres completos de los nodos
  full_names <- rownames(network$graph)
  if (is.null(full_names)) full_names <- colnames(network$graph)

  # Determinar etiquetas a usar
  if (!is.null(labels)) {
    if (length(labels) != length(full_names)) {
      stop(paste("La longitud de 'labels' (", length(labels),
                 ") debe coincidir con el n\u00famero de nodos (",
                 length(full_names), ")"))
    }
    qgraph_labels <- labels
  } else {
    qgraph_labels <- qgraph_obj$graphAttributes$Nodes$labels
  }

  # Crear mapeo de nombres
  name_to_abbrev <- setNames(qgraph_labels, full_names)

  # --- Calcular centralidad principal (measure0) ---
  ctbl <- centralityTable(network)

  cents_main <- ctbl %>%
    filter(measure == !!measure0) %>%
    select(full_name = node, value) %>%
    arrange(full_name) %>%
    rename(!!measure0 := value) %>%
    mutate(Abrev = name_to_abbrev[full_name])

  # --- Calcular medida de puente (measure1) ---
  cents_bridge <- NULL

  if (!is.null(measure1) && !is.null(groups)) {
    tryCatch({
      b_obj <- bridge(qgraph_obj,
                      communities = groups,
                      useCommunities = "all",
                      normalize = FALSE)

      if (!is.null(b_obj) && measure1 %in% names(b_obj)) {
        bridge_values <- b_obj[[measure1]]

        if (!is.null(bridge_values) && length(bridge_values) == length(qgraph_labels)) {
          cents_bridge <- tibble(
            qgraph_label = qgraph_labels,
            raw_bridge = as.numeric(bridge_values)
          ) %>%
            mutate(!!measure1 := as.numeric(scale(raw_bridge))) %>%
            select(-raw_bridge)

          # Mapear nombres completos
          name_mapping <- tibble(
            full_name = full_names,
            qgraph_label = qgraph_labels
          )

          cents_bridge <- cents_bridge %>%
            left_join(name_mapping, by = "qgraph_label") %>%
            select(full_name, !!sym(measure1))
        }
      }
    }, error = function(e) {
      warning(paste("Error al calcular medidas de puente:", e$message))
    })
  }

  # --- Combinar datos ---
  if (!is.null(cents_bridge)) {
    plot_data <- cents_main %>%
      left_join(cents_bridge, by = "full_name") %>%
      mutate(difference = !!sym(measure0) - !!sym(measure1))
  } else {
    plot_data <- cents_main %>%
      mutate(!!measure1 := NA_real_,
             difference = NA_real_)
  }

  # Determinar el orden
  if (order_by == "measure0") {
    plot_data <- plot_data %>% arrange(!!sym(measure0))
  } else if (order_by == "measure1" && !is.null(cents_bridge)) {
    plot_data <- plot_data %>% arrange(!!sym(measure1))
  } else if (order_by == "difference" && !is.null(cents_bridge)) {
    plot_data <- plot_data %>% arrange(difference)
  } else {
    plot_data <- plot_data %>% arrange(desc(Abrev))
  }

  # Variable para el eje Y
  y_var <- if (use_abbrev) "Abrev" else "full_name"
  plot_data$y_label <- factor(plot_data[[y_var]], levels = plot_data[[y_var]])

  # --- Crear el Cleveland Dot Plot ---
  p <- ggplot(plot_data, aes(y = y_label))

  # Linea vertical en cero si se solicita
  if (show_zero_line) {
    p <- p + geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.5)
  }

  # Si hay dos metricas, agregar segmento de conexion
  if (!is.null(cents_bridge)) {
    p <- p +
      # Segmento que conecta ambos puntos
      geom_segment(aes(x = !!sym(measure0),
                       xend = !!sym(measure1),
                       yend = y_label),
                   color = segment_color,
                   linewidth = segment_size)
  }

  # Segmentos desde el eje Y hasta los puntos (estilo Cleveland clasico)
  p <- p +
    geom_segment(aes(x = 0, xend = !!sym(measure0), yend = y_label),
                 color = color0, linewidth = 0.3, alpha = 0.4)

  # Puntos para measure0
  p <- p +
    geom_point(aes(x = !!sym(measure0)),
               color = color0,
               size = point_size,
               shape = 16)

  # Puntos para measure1 si existe
  if (!is.null(cents_bridge)) {
    p <- p +
      geom_point(aes(x = !!sym(measure1)),
                 color = color1,
                 size = point_size,
                 shape = 17)  # Triangulo para diferenciar
  }

  # Tema y etiquetas
  p <- p +
    labs(
      x = "z-score",
      y = NULL
    ) +
    theme_minimal(base_size = text_size) +
    theme(
      axis.text.y = element_text(size = text_size, face = "bold", color = "grey20"),
      axis.text.x = element_text(size = text_size - 1),
      axis.title.x = element_text(size = text_size, face = "bold", margin = margin(t = 10)),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = element_text(size = text_size, face = "bold"),
      legend.text = element_text(size = text_size - 1),
      plot.margin = margin(10, 15, 10, 10)
    )

  # Crear leyenda manual con puntos
  if (!is.null(cents_bridge)) {
    # Crear datos para leyenda
    legend_df <- data.frame(
      metric = factor(c(measure0, measure1), levels = c(measure0, measure1)),
      x = c(1, 2),
      y = c(1, 1)
    )

    # Agregar leyenda como capa invisible + override
    p <- p +
      geom_point(data = legend_df,
                 aes(x = x, y = 1, color = metric, shape = metric),
                 size = 0, show.legend = TRUE) +
      scale_color_manual(
        name = "M\u00e9trica",
        values = setNames(c(color0, color1), c(measure0, measure1)),
        labels = c(measure0, measure1)
      ) +
      scale_shape_manual(
        name = "M\u00e9trica",
        values = setNames(c(16, 17), c(measure0, measure1)),
        labels = c(measure0, measure1)
      ) +
      guides(
        color = guide_legend(override.aes = list(size = point_size)),
        shape = guide_legend(override.aes = list(size = point_size))
      )
  }

  # Suprimir warnings
  class(p) <- c("silent_gg", class(p))
  assign("print.silent_gg",
         function(x, ...) suppressWarnings(NextMethod()),
         envir = .GlobalEnv)

  # Preparar tabla de salida
  output_table <- plot_data %>%
    select(full_name, Abrev, !!sym(measure0), any_of(measure1)) %>%
    arrange(desc(!!sym(measure0)))

  return(list(
    plot = p,
    table = output_table
  ))
}
