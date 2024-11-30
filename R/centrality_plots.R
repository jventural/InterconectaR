centrality_plots <- function(qgraph_obj, network, measure0 = "ExpectedInfluence", measure1 = "Bridge Expected Influence (1-step)") {

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
    mutate(measure1 = measure1) %>%
    mutate(V1 = scale(V1))

  # Unir las tablas de centralidad y seleccionar columnas clave
  cents2 <- bind_cols(bridge_data, cents_expect) %>%
    select(node, Item, V1, value) %>%
    rename(value_BEI = V1, value_EI = value)

  # Preparar los datos para el gráfico combinado
  cents_long <- cents2 %>%
    pivot_longer(cols = c("value_BEI", "value_EI"),
                 names_to = "Measure",
                 values_to = "Value") %>%
    mutate(Measure = recode(Measure,
                            "value_BEI" = "Bridge Expected Influence",
                            "value_EI" = "Expected Influence")) %>%
    rename(Centrality = Measure)

  # Crear el gráfico combinado
  Figura1_Derecha <- ggplot(cents_long, aes(x = Value, y = reorder(Item, Value),
                                            color = Centrality, group = Centrality)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(title = "",
         x = "Value",
         y = "Item",
         color = "Centrality") +  # Etiqueta para la leyenda
    scale_color_manual(values = c(
      "Bridge Expected Influence" = "#FF0000",
      "Expected Influence" = "#00A08A"
    )) +
    theme(axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15))

  # Retornar los resultados
  list(
    table = cents2 %>% arrange(desc(value_EI)),
    plot = Figura1_Derecha
  )
}
