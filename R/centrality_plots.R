centrality_plots <- function(qgraph_obj, network,
                             measure0 = "ExpectedInfluence",
                             measure1 = "Bridge Expected Influence (1-step)",
                             color_palette = c("#FF0000", "#00A08A"),
                             labels = NULL) {
  # Requerir las librerías necesarias
  if (!require("qgraph", quietly = TRUE)) install.packages("qgraph", dependencies = TRUE)
  if (!require("dplyr", quietly = TRUE)) install.packages("dplyr", dependencies = TRUE)
  if (!require("networktools", quietly = TRUE)) install.packages("networktools", dependencies = TRUE)
  if (!require("tibble", quietly = TRUE)) install.packages("tibble", dependencies = TRUE)
  if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2", dependencies = TRUE)
  if (!require("tidyr", quietly = TRUE)) install.packages("tidyr", dependencies = TRUE)

  library(qgraph)
  library(dplyr)
  library(networktools)
  library(tibble)
  library(ggplot2)
  library(tidyr)

  # Calcular la tabla de centralidad de la influencia esperada
  cents_expect <- qgraph::centralityTable(network) %>%
    filter(measure == !!measure0)

  # Calcular la influencia de puente
  b <- bridge(qgraph_obj, communities = groups, useCommunities = "all", normalize = FALSE)

  # Convertir el resultado en un data frame
  bridge_data <- as.data.frame(cbind(b[[measure1]])) %>%
    rownames_to_column(var = "Item") %>%
    mutate(!!sym(measure1) := scale(V1)) %>%
    select(-V1)

  # Aplicar etiquetas personalizadas si se proporcionan
  if (!is.null(labels)) {
    bridge_data <- bridge_data %>%
      mutate(Item = ifelse(Item %in% names(labels), labels[Item], Item))
  }

  # Unir las tablas de centralidad y seleccionar columnas clave
  cents2 <- bind_cols(bridge_data, cents_expect) %>%
    select(node, Item, !!sym(measure1), value) %>%
    rename(!!measure0 := value)

  # Preparar los datos para el gráfico combinado
  cents_long <- cents2 %>%
    pivot_longer(cols = c(!!sym(measure1), !!sym(measure0)),
                 names_to = "Measure",
                 values_to = "Value") %>%
    rename(Centrality = Measure)

  # Crear el gráfico combinado
  Figura1_Derecha <- ggplot(cents_long, aes(x = Value, y = reorder(Item, Value),
                                            color = Centrality, group = Centrality)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(title = "",
         x = "z-score",
         y = "Nodes",
         color = "Centrality") +  # Etiqueta para la leyenda
    scale_color_manual(values = setNames(color_palette, c(measure1, measure0))) +
    theme(axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title.y = element_text(size = 20),
          legend.position = "bottom")

  # Retornar los resultados
  list(
    table = cents2 %>% arrange(desc(!!sym(measure0))),
    plot = Figura1_Derecha
  )
}
