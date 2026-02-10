#' Bullet Chart para Indices de Centralidad
#'
#' Crea un grafico tipo Bullet Chart para visualizar dos metricas de centralidad
#' (por ejemplo, Strength y Bridge Strength) de forma compacta y comparativa.
#'
#' @param qgraph_obj Objeto qgraph creado previamente.
#' @param network Objeto de red estimado (por ejemplo, de bootnet::estimateNetwork).
#' @param groups Lista de grupos/comunidades para calcular metricas de puente.
#' @param measure0 Metrica principal (barra). Default: "Strength".
#' @param measure1 Metrica secundaria (linea marcador). Default: "Bridge Strength".
#' @param color_bar Color de la barra principal. Default: "#3498DB".
#' @param color_marker Color del marcador de referencia. Default: "#E74C3C".
#' @param color_background Color de fondo de la barra. Default: "#ECF0F1".
#' @param bar_height Altura de las barras (0-1). Default: 0.6.
#' @param marker_size Tamano del marcador triangular. Default: 4.
#' @param use_abbrev Usar nombres abreviados. Default: TRUE.
#' @param labels Vector de etiquetas personalizadas para los nodos. Default: NULL.
#' @param order_by Ordenar por: "measure0", "measure1", "name". Default: "measure0".
#' @param show_values Mostrar valores numericos. Default: TRUE.
#' @param text_size Tamano del texto de los nodos. Default: 11.
#'
#' @return Lista con dos elementos:
#'   \item{plot}{Objeto ggplot con el Bullet Chart.}
#'   \item{table}{Tabla con los valores de centralidad.}
#'
#' @examples
#' \dontrun{
#' result <- centrality_bullet_plot(
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
#' @importFrom ggplot2 ggplot aes geom_tile geom_point geom_segment geom_text annotate coord_cartesian scale_x_continuous scale_color_manual scale_shape_manual guides guide_legend labs theme_minimal theme element_text element_blank element_rect element_line expansion margin
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom rlang sym !! :=
centrality_bullet_plot <- function(qgraph_obj,
                                   network,
                                   groups = NULL,
                                   measure0 = "Strength",
                                   measure1 = "Bridge Strength",
                                   color_bar = "#3498DB",
                                   color_marker = "#E74C3C",
                                   color_background = "#ECF0F1",
                                   bar_height = 0.6,
                                   marker_size = 4,
                                   use_abbrev = TRUE,
                                   labels = NULL,
                                   order_by = "measure0",
                                   show_values = TRUE,
                                   text_size = 11) {

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
      left_join(cents_bridge, by = "full_name")
  } else {
    plot_data <- cents_main %>%
      mutate(!!measure1 := NA_real_)
  }

  # Determinar el orden
  if (order_by == "measure0") {
    plot_data <- plot_data %>% arrange(desc(!!sym(measure0)))
  } else if (order_by == "measure1" && !is.null(cents_bridge)) {
    plot_data <- plot_data %>% arrange(desc(!!sym(measure1)))
  } else {
    plot_data <- plot_data %>% arrange(Abrev)
  }

  # Variable para el eje Y
  y_var <- if (use_abbrev) "Abrev" else "full_name"
  plot_data$y_label <- factor(plot_data[[y_var]], levels = rev(plot_data[[y_var]]))

  # Calcular rango para el fondo
  x_min <- min(c(plot_data[[measure0]], plot_data[[measure1]]), na.rm = TRUE)
  x_max <- max(c(plot_data[[measure0]], plot_data[[measure1]]), na.rm = TRUE)
  x_range <- x_max - x_min
  x_buffer <- x_range * 0.1

  # --- Crear el Bullet Chart ---
  p <- ggplot(plot_data, aes(y = y_label)) +
    # Barra de fondo (rango completo)
    geom_tile(aes(x = (x_min + x_max) / 2,
                  width = x_max - x_min + x_buffer * 2,
                  height = bar_height + 0.15),
              fill = color_background,
              alpha = 0.5) +
    # Barra principal (measure0)
    geom_tile(aes(x = !!sym(measure0) / 2 + x_min / 2,
                  width = !!sym(measure0) - x_min,
                  height = bar_height),
              fill = color_bar,
              alpha = 0.85)

  # Agregar marcador para measure1 si existe
  if (!is.null(cents_bridge)) {
    p <- p +
      # Linea vertical como marcador
      geom_point(aes(x = !!sym(measure1)),
                 shape = 18,  # Diamante
                 size = marker_size,
                 color = color_marker) +
      geom_segment(aes(x = !!sym(measure1),
                       xend = !!sym(measure1),
                       y = as.numeric(y_label) - bar_height/2.5,
                       yend = as.numeric(y_label) + bar_height/2.5),
                   color = color_marker,
                   linewidth = 1.5)
  }

  # Agregar valores numericos si se solicita
  if (show_values) {
    p <- p +
      geom_text(aes(x = x_max + x_buffer * 0.8,
                    label = sprintf("%.2f", !!sym(measure0))),
                hjust = 0,
                size = text_size / 3,
                color = color_bar,
                fontface = "bold")

    if (!is.null(cents_bridge)) {
      p <- p +
        geom_text(aes(x = x_max + x_buffer * 2.2,
                      label = sprintf("%.2f", !!sym(measure1))),
                  hjust = 0,
                  size = text_size / 3,
                  color = color_marker,
                  fontface = "bold")
    }
  }

  # Tema y etiquetas
  p <- p +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    labs(
      x = "z-score",
      y = NULL,
      title = NULL
    ) +
    theme_minimal(base_size = text_size) +
    theme(
      axis.text.y = element_text(size = text_size, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = text_size - 1),
      axis.title.x = element_text(size = text_size, face = "bold", margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linetype = "dashed"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )

  # Agregar leyenda manual
  legend_data <- data.frame(
    x = c(x_min, x_min + x_range * 0.3),
    y = c(0.3, 0.3),
    label = c(measure0, measure1),
    color = c(color_bar, color_marker)
  )

  # Crear leyenda como anotacion
  p <- p +
    annotate("rect",
             xmin = x_min - x_buffer * 0.5,
             xmax = x_min + x_range * 0.15,
             ymin = -0.2, ymax = 0.2,
             fill = color_bar, alpha = 0.85) +
    annotate("text",
             x = x_min + x_range * 0.17,
             y = 0,
             label = measure0,
             hjust = 0, size = text_size / 3,
             fontface = "bold", color = "grey30") +
    annotate("point",
             x = x_min + x_range * 0.45,
             y = 0,
             shape = 18, size = marker_size,
             color = color_marker) +
    annotate("text",
             x = x_min + x_range * 0.48,
             y = 0,
             label = measure1,
             hjust = 0, size = text_size / 3,
             fontface = "bold", color = "grey30") +
    coord_cartesian(ylim = c(0.5, nrow(plot_data) + 0.5), clip = "off")

  # Suprimir warnings
  class(p) <- c("silent_gg", class(p))
  assign("print.silent_gg",
         function(x, ...) suppressWarnings(NextMethod()),
         envir = .GlobalEnv)

  # Preparar tabla de salida
  output_table <- plot_data %>%
    select(full_name, Abrev, !!sym(measure0), !!sym(measure1)) %>%
    arrange(desc(!!sym(measure0)))

  return(list(
    plot = p,
    table = output_table
  ))
}
