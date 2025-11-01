plot_centrality_by_group <- function(networks_groups, replacements, measure_spec,
                                     color_palette = c("#FF5733", "#33FFCE")) {
  library(dplyr)
  library(ggplot2)
  library(forcats)

  # 1) Calcular tabla de centralidad
  cents_raw <- centralityTable(networks_groups[[1]]$graph,
                               networks_groups[[2]]$graph)

  cents <- cents_raw %>%
    rename(group = graph) %>%
    mutate(group = dplyr::case_when(
      group == "graph 1" ~ replacements[1],
      group == "graph 2" ~ replacements[2],
      TRUE ~ group
    )) %>%
    select(-type) %>%
    filter(measure == measure_spec)

  if (nrow(cents) == 0) {
    stop(
      paste0("No hay filas para measure = '", measure_spec,
             "'. Medidas disponibles: ",
             paste(unique(cents_raw$measure), collapse = ", "))
    )
  }

  # Orden de nodos según media (como en el plot)
  node_order <- cents %>%
    group_by(node) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_value) %>%
    pull(node)

  cents <- cents %>%
    mutate(node = factor(node, levels = node_order))

  # 2) Gráfico
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
