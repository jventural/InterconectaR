centrality_bridge_plot <- function(
    networks_groups,
    group_names = c("Mujer", "Varón"),
    measure = "Bridge Expected Influence (1-step)",
    color_palette = c("#FF5733", "#33FFCE")
) {
  # Librerías necesarias
  library(qgraph)
  library(dplyr)
  library(networktools)
  library(tibble)
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(forcats)

  # Extraer las redes de los grupos seleccionados
  network1 <- networks_groups[[group_names[1]]]
  network2 <- networks_groups[[group_names[2]]]

  # Calcular la influencia de puente para ambas redes (sin graficar las redes)
  qgraph_obj1 <- qgraph(network1$graph, DoNotPlot = TRUE)
  qgraph_obj2 <- qgraph(network2$graph, DoNotPlot = TRUE)

  bridge1 <- bridge(qgraph_obj1, communities = groups, useCommunities = "all", normalize = FALSE)
  bridge2 <- bridge(qgraph_obj2, communities = groups, useCommunities = "all", normalize = FALSE)

  # Crear la tabla combinada de centralidades puente
  centrality_data <- as.data.frame(cbind(bridge1[[measure]], bridge2[[measure]])) %>%
    rownames_to_column(var = "Symptoms") %>%
    rename_at(vars(V1, V2), ~ c(group_names[1], group_names[2])) %>%
    reshape2::melt(id = "Symptoms") %>%
    rename(Centrality = variable) %>%
    mutate(zscore = scale(value)) %>%
    mutate(across(where(is.numeric), round, 2))

  # Crear el gráfico combinado de centralidades puente con colores
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

  # Retornar solo la tabla y el gráfico
  return(list(
    table = centrality_data,
    plot  = plot
  ))
}
