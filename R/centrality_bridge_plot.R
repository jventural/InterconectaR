#' Bridge Centrality Plot
#'
#' Creates a comparative bridge centrality plot for two groups.
#'
#' @param networks_groups Named list of network objects by group.
#' @param group_names Character vector of length 2 with group names.
#' @param measure Bridge centrality measure name.
#' @param color_palette Character vector of 2 colors for the groups.
#' @param communities Community membership vector for bridge calculation.
#'
#' @export
#' @importFrom qgraph qgraph
#' @importFrom dplyr %>% rename_at mutate across
#' @importFrom networktools bridge
#' @importFrom tibble rownames_to_column
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_shape_manual scale_color_manual xlab ylab theme_bw coord_flip labs theme element_text element_blank
#' @importFrom reshape2 melt
#' @importFrom forcats fct_reorder
#' @importFrom tidyselect where
centrality_bridge_plot <- function(
    networks_groups,
    group_names = c("Mujer", "Var\u00f3n"),
    measure = "Bridge Expected Influence (1-step)",
    color_palette = c("#FF5733", "#33FFCE"),
    communities = NULL
) {
  # Extraer las redes de los grupos seleccionados
  network1 <- networks_groups[[group_names[1]]]
  network2 <- networks_groups[[group_names[2]]]

  # Calcular la influencia de puente para ambas redes (sin graficar las redes)
  qgraph_obj1 <- qgraph(network1$graph, labels = colnames(network1$graph), DoNotPlot = TRUE)
  qgraph_obj2 <- qgraph(network2$graph, labels = colnames(network2$graph), DoNotPlot = TRUE)

  bridge1 <- bridge(qgraph_obj1, communities = communities, useCommunities = "all", normalize = FALSE)
  bridge2 <- bridge(qgraph_obj2, communities = communities, useCommunities = "all", normalize = FALSE)

  # Crear la tabla combinada de centralidades puente
  centrality_data <- as.data.frame(cbind(bridge1[[measure]], bridge2[[measure]])) %>%
    rownames_to_column(var = "Symptoms") %>%
    rename_at(vars(V1, V2), ~ c(group_names[1], group_names[2])) %>%
    reshape2::melt(id = "Symptoms") %>%
    rename(Centrality = variable) %>%
    mutate(zscore = scale(value)) %>%
    mutate(across(where(is.numeric), round, 2))

  # Crear el grafico combinado de centralidades puente con colores
  plot <- ggplot(centrality_data, aes(
    x = forcats::fct_reorder(factor(Symptoms), zscore, mean),
    y = zscore,
    group = Centrality,
    color = Centrality
  )) +
    geom_line(aes(linetype = Centrality, color = Centrality)) +
    geom_point(aes(shape = Centrality, color = Centrality), size = 3) +
    scale_shape_manual(values = c(8, 13)) +
    scale_color_manual(values = color_palette) +
    xlab("Nodes") +
    ylab("z-score") +
    theme_bw() +
    coord_flip() +
    labs(
      title = paste(measure),
      linetype = "Group",
      shape = "Group",
      color = "Group"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )

  # Retornar solo la tabla y el grafico
  return(list(
    table = centrality_data,
    plot  = plot
  ))
}
