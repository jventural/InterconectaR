plot_centrality_by_group <- function(networks_groups, replacements, measure_spec,
                                     color_palette = c("#FF5733", "#33FFCE"),
                                     labels = NULL) {
  library(dplyr)
  library(ggplot2)
  library(forcats)

  # Generar la tabla de centralidad y ajustarla
  cents <- centralityTable(networks_groups[[1]]$graph, networks_groups[[2]]$graph) %>%
    rename(group = graph) %>%
    mutate(group = case_when(
      group == "graph 1" ~ replacements[1],
      group == "graph 2" ~ replacements[2],
      TRUE ~ group
    )) %>%
    select(-type) %>%
    filter(measure == measure_spec)

  # Modificar los valores de 'node' si se proporcionan etiquetas personalizadas
  if (!is.null(labels)) {
    cents <- cents %>%
      mutate(node = ifelse(node %in% names(labels), labels[node], as.character(node)))
  }

  # Crear el gráfico utilizando ggplot
  Figure2 <- ggplot(data = cents,
                    aes(x = forcats::fct_reorder(factor(node), value, mean),
                        y = value,
                        group = group,
                        color = group)) +
    geom_line(aes(linetype = group, color = group)) +
    geom_point(aes(shape = group, color = group), size = 3) +
    scale_shape_manual(values = c(8, 13)) +  # Personaliza las formas de los puntos
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    xlab("Nodes") +
    ylab("z-score") +
    theme_bw() +
    coord_flip() +
    labs(title = paste(unique(cents$measure)),
         linetype = "Group",
         shape = "Group",
         color = "Group") +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )

  # Retornar el gráfico creado
  return(Figure2)
}
