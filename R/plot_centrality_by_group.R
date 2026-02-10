#' Plot centrality measures by group
#'
#' @param networks_groups A named list of two network objects.
#' @param replacements Character vector of length 2 with group replacement names.
#' @param measure_spec Character string specifying the centrality measure.
#' @param color_palette Character vector of two colors for the groups.
#'
#' @return A list with elements \code{plot} (a ggplot object) and \code{table} (a data frame).
#' @export
#' @importFrom dplyr %>% rename mutate case_when select filter group_by summarise arrange pull
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_shape_manual scale_color_manual xlab ylab theme_bw coord_flip labs theme element_text element_blank
#' @importFrom forcats fct_reorder
#' @importFrom qgraph centralityTable
plot_centrality_by_group <- function(networks_groups, replacements, measure_spec,
                                     color_palette = c("#FF5733", "#33FFCE")) {

 # 1) Obtener nombres de los grupos
  group_names <- names(networks_groups)

  # 2) Calcular tabla de centralidad pasando los objetos de red completos
  cents_raw <- centralityTable(networks_groups[[1]],
                               networks_groups[[2]])

  cents <- cents_raw %>%
    dplyr::rename(group = graph) %>%
    dplyr::mutate(group = dplyr::case_when(
      group == "graph 1" ~ replacements[1],
      group == "graph 2" ~ replacements[2],
      TRUE ~ group
    )) %>%
    dplyr::select(-type) %>%
    dplyr::filter(measure == measure_spec)

  if (nrow(cents) == 0) {
    stop(
      paste0("No hay filas para measure = '", measure_spec,
             "'. Medidas disponibles: ",
             paste(unique(cents_raw$measure), collapse = ", "))
    )
  }

  # Orden de nodos segun media (como en el plot)
  node_order <- cents %>%
    group_by(node) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_value) %>%
    pull(node)

  cents <- cents %>%
    mutate(node = factor(node, levels = node_order))

  # 2) Grafico
  Figure2 <- ggplot(
    data = cents,
    aes(
      x = node,
      y = value,
      group = group,
      color = group
    )
  ) +
    geom_line(aes(linetype = group)) +
    geom_point(aes(shape = group), size = 3) +
    scale_shape_manual(values = c(8, 13)) +
    scale_color_manual(values = color_palette) +
    xlab("Nodes") +
    ylab("z-score") +
    theme_bw() +
    coord_flip() +
    labs(
      title = paste(unique(cents$measure)),
      linetype = "Group",
      shape   = "Group",
      color   = "Group"
    ) +
    theme(
      axis.text.y    = element_text(size = 12),
      axis.text.x    = element_text(size = 12),
      legend.text    = element_text(size = 10),
      legend.title   = element_text(size = 12),
      panel.grid.minor = element_blank()
    )

  # 3) Salida como lista
  out_table <- cents %>%
    arrange(node, group) %>%
    mutate(node = as.character(node))

  return(list(
    plot  = Figure2,
    table = out_table
  ))
}
