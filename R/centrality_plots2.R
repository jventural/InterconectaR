#' Centrality Plot (Version 2)
#'
#' Creates centrality plots with optional bridge metrics.
#'
#' @param qgraph_obj A qgraph object.
#' @param network Estimated network object.
#' @param groups List of communities for bridge calculation.
#' @param measure0 Primary centrality measure name.
#' @param measure1 Secondary bridge measure name (NULL for single measure).
#' @param color_palette Character vector of colors.
#' @param labels Named vector mapping node names to custom labels.
#' @param legend_labels Custom legend labels.
#' @param use_abbrev Logical; use abbreviated node names.
#'
#' @return A list with `table` (a data frame with the centrality values of
#'   each node) and `plot` (a ggplot object).
#' @export
#' @examples
#' set.seed(123)
#' n <- 300
#' f1 <- rnorm(n)
#' f2 <- 0.3 * f1 + rnorm(n)
#' items <- data.frame(
#'   sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'   sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#' )
#' names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
#'
#' net <- bootnet::estimateNetwork(items, default = "EBICglasso")
#' g <- qgraph::qgraph(net$graph, DoNotPlot = TRUE)
#' res <- centrality_plots2(g, net,
#'                          groups = rep(c("A", "B"), each = 4),
#'                          measure1 = "Bridge Expected Influence (1-step)")
#' res$table
#' res$plot
#' @importFrom qgraph centralityTable
#' @importFrom dplyr %>% filter select arrange rename mutate inner_join desc if_else
#' @importFrom networktools bridge
#' @importFrom tibble enframe
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual labs theme_minimal theme element_text
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom rlang quo sym !! :=
centrality_plots2 <- function(qgraph_obj,
                              network,
                              groups = NULL,
                              measure0 = "ExpectedInfluence",
                              measure1 = NULL,
                              color_palette = c("#FF0000", "#00A08A"),
                              labels = NULL,
                              legend_labels = NULL,
                              use_abbrev = TRUE) {
  ################################################################################
  # 2) Extraer tabla de centralidad para measure0
  ################################################################################
  ctbl <- centralityTable(network)

  cents_expect <- ctbl %>%
    dplyr::filter(measure == !!measure0) %>%
    dplyr::select(node_name = node, value) %>%
    dplyr::arrange(node_name) %>%
    dplyr::rename(!!measure0 := value)

  # Guardar nombres originales para el join con bridge

  cents_expect <- cents_expect %>%
    dplyr::mutate(original_name = node_name)

  # Aplicar etiquetas personalizadas si las hay

  if (!is.null(labels)) {
    cents_expect <- cents_expect %>%
      dplyr::mutate(display_name = if_else(node_name %in% names(labels),
                                    labels[node_name],
                                    node_name))
  } else {
    cents_expect <- cents_expect %>%
      dplyr::mutate(display_name = node_name)
  }

  ################################################################################
  # 3) Si measure1 no es NULL: calcular puente
  ################################################################################
  if (!is.null(measure1)) {
    # 3.1) Ejecutar bridge()
    b_obj <- bridge(
      qgraph_obj,
      communities    = groups,
      useCommunities = "all",
      normalize      = FALSE
    )

    # 3.2) Convertir b_obj[[measure1]] en data.frame usando nombres originales
    bridge_data <- tibble::enframe(
      b_obj[[measure1]],
      name  = "original_name",
      value = "raw_bridge"
    ) %>%
      dplyr::mutate(!!measure1 := as.numeric(scale(raw_bridge))) %>%
      dplyr::select(-raw_bridge) %>%
      dplyr::arrange(original_name)

    # 3.3) Unir bridge_data con cents_expect por "original_name"
    cents2 <- dplyr::inner_join(
      bridge_data,
      cents_expect,
      by = "original_name"
    ) %>%
      dplyr::select(original_name, display_name, !!sym(measure0), !!sym(measure1))

    ################################################################################
    # 4A) Preparar datos en formato "largo" para ggplot (dos metricas)
    ################################################################################
    # Definir que variable usaremos en el eje y:
    y_var <- if (use_abbrev) quo(original_name) else quo(display_name)

    cents_long <- cents2 %>%
      tidyr::pivot_longer(
        cols      = c(!!sym(measure0), !!sym(measure1)),
        names_to  = "Centrality",
        values_to = "Value"
      ) %>%
      dplyr::rename(Measure = Centrality)

    ################################################################################
    # 5A) Definir paleta y etiquetas de leyenda
    ################################################################################
    if (is.null(legend_labels)) {
      pal         <- setNames(color_palette, c(measure1, measure0))
      legend_lbls <- setNames(c(measure1, measure0), c(measure1, measure0))
    } else {
      pal         <- setNames(color_palette, c(measure1, measure0))
      legend_lbls <- legend_labels
    }

    ################################################################################
    # 6A) Construir ggplot con dos lineas
    ################################################################################
    Figura <- ggplot(
      cents_long,
      aes(
        x     = Value,
        y     = fct_reorder(!!y_var, Value),
        color = Measure,
        group = Measure
      )
    ) +
      geom_point(size = 3) +
      geom_line(linewidth = 0.5) +
      theme_minimal() +
      labs(
        x     = "z-score",
        y     = "Nodos",
        color = "M\u00e9trica"
      ) +
      scale_color_manual(
        values = pal,
        labels = legend_lbls
      ) +
      theme(
        axis.text.y     = element_text(size = 12),
        axis.text.x     = element_text(size = 12),
        legend.text     = element_text(size = 12),
        legend.title    = element_text(size = 12),
        axis.title.y    = element_text(size = 12),
        axis.title.x    = element_text(size = 12),
        legend.position = "bottom"
      )

    ################################################################################
    # 7A) Devolver tabla y grafico (caso dual)
    ################################################################################
    return(
      list(
        table = cents2 %>% dplyr::arrange(desc(!!sym(measure0))),
        plot  = Figura
      )
    )
  }

  ################################################################################
  # 4B) Caso measure1 = NULL: solo measure0
  ################################################################################
  cents_single <- cents_expect %>%
    dplyr::select(original_name, display_name, !!sym(measure0)) %>%
    dplyr::arrange(desc(!!sym(measure0))) %>%
    dplyr::rename(Value = !!sym(measure0))

  ################################################################################
  # 5B) Preparar paleta y leyenda para una sola metrica
  ################################################################################
  pal_single         <- setNames(color_palette[1], measure0)
  legend_lbls_single <- setNames(measure0, measure0)

  ################################################################################
  # 6B) Construir ggplot para solo measure0 (linea + puntos)
  ################################################################################
  y_var_single <- if (use_abbrev) quo(original_name) else quo(display_name)

  Figura <- ggplot(
    cents_single,
    aes(
      x     = Value,
      y     = fct_reorder(!!y_var_single, Value),
      group = 1
    )
  ) +
    geom_line(color = pal_single, linewidth = 0.5) +
    geom_point(color = pal_single, size = 3) +
    theme_minimal() +
    labs(
      x     = "z-score",
      y     = "Nodos",
      color = "M\u00e9trica"
    ) +
    scale_color_manual(
      values = pal_single,
      labels = legend_lbls_single
    ) +
    theme(
      axis.text.y     = element_text(size = 12),
      axis.text.x     = element_text(size = 12),
      legend.text     = element_text(size = 12),
      legend.title    = element_text(size = 12),
      axis.title.y    = element_text(size = 12),
      axis.title.x    = element_text(size = 12),
      legend.position = "bottom"
    )

  ################################################################################
  # 7B) Devolver tabla y grafico (caso single)
  ################################################################################
  return(
    list(
      table = cents_single,
      plot  = Figura
    )
  )
}
