get_edge_weights_summary <- function(network) {
  # Obtener la matriz de pesos de los bordes y redondearla
  edge.matrix <- round(getWmat(getWmat(network$graph)), digits = 2)

  # Aplanar la matriz a un vector para análisis estadístico
  edge.values <- as.vector(edge.matrix)

  # Calcular descriptivos
  mean_value <- mean(edge.values)
  sd_value <- sd(edge.values)
  min_value <- min(edge.values)
  max_value <- max(edge.values)

  # Retornar los resultados en un dataframe
  summary_df <- data.frame(
    Mean = mean_value,
    SD = sd_value,
    Min = min_value,
    Max = max_value
  )

  return(summary_df)
}
