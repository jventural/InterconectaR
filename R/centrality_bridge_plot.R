centrality_bridge_plot <- function(
    networks_groups,
    group_names = c("Mujer", "Varón"),
    groups,
    measure0 = "ExpectedInfluence",
    measure1 = "Bridge Expected Influence (1-step)",
    color_palette = c("#FF5733", "#33FFCE"),
    layout_type = "spring", # Layout por defecto
    layout_scale = c(0.8, 0.8),
    vsize = 12,
    esize = 12,
    legend_cex = 0.5,
    edge_labels = TRUE,
    labels = NULL
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

  # Asignar etiquetas personalizadas si se proporcionan
  node_labels <- if (is.null(labels)) {
    network1$labels # Usar etiquetas por defecto
  } else {
    labels # Usar etiquetas proporcionadas
  }

  # Construir los gráficos para ambos grupos
  qgraph_obj1 <- qgraph(network1$graph,
                        groups = groups,
                        curveAll = 2,
                        vsize = vsize,
                        esize = esize,
                        palette = "ggplot2",
                        layout = layout_type,
                        edge.labels = edge_labels,
                        labels = node_labels, # Aplicar etiquetas
                        layoutScale = layout_scale,
                        legend.cex = legend_cex)

  qgraph_obj2 <- qgraph(network2$graph,
                        groups = groups,
                        curveAll = 2,
                        vsize = vsize,
                        esize = esize,
                        palette = "ggplot2",
                        layout = layout_type,
                        edge.labels = edge_labels,
                        labels = node_labels, # Aplicar etiquetas
                        layoutScale = layout_scale,
                        legend.cex = legend_cex)

  # Calcular la influencia de puente para ambas redes
  bridge1 <- bridge(qgraph_obj1, communities = groups, useCommunities = "all", normalize = FALSE)
  bridge2 <- bridge(qgraph_obj2, communities = groups, useCommunities = "all", normalize = FALSE)

  # Crear la tabla combinada de centralidades puente
  centrality_data <- as.data.frame(cbind(bridge1[[measure1]], bridge2[[measure1]])) %>%
    rownames_to_column(var = "Symptoms") %>%
    rename_at(vars(V1, V2), ~ c(group_names[1], group_names[2])) %>% # Nombres cortos de los grupos
    reshape2::melt(id = "Symptoms") %>%
    rename(Centrality = variable) %>%
    mutate(zscore = scale(value)) %>%
    mutate(across(where(is.numeric), round, 2))

  # Crear el gráfico combinado de centralidades puente con colores
  plot <- ggplot(centrality_data, aes(x = forcats::fct_reorder(factor(Symptoms), zscore, mean),
                                      y = zscore,
                                      group = Centrality,
                                      color = Centrality)) +
    geom_line(aes(linetype = Centrality, color = Centrality)) +
    geom_point(aes(shape = Centrality, color = Centrality), size = 3) +
    scale_shape_manual(values = c(8, 13)) +
    scale_color_manual(values = color_palette) + # Usar la paleta de colores proporcionada
    xlab("Nodes") + ylab("z-score") +
    theme_bw() +
    coord_flip() +
    labs(title = paste(measure1), # Título dinámico
         linetype = "Group",
         shape = "Group",
         color = "Group") +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    )

  # Retornar los objetos de red, la tabla y el gráfico
  return(list(
    qgraph_obj1 = qgraph_obj1,
    qgraph_obj2 = qgraph_obj2,
    table = centrality_data,
    plot = plot
  ))
}
