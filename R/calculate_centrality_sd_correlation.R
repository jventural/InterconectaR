calculate_centrality_sd_correlation <- function(Data, Centralitys) {
  # Verificar e instalar librerías necesarias
  required_packages <- c("dplyr", "tibble", "correlation")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Calcular el SD y convertirlo en un data frame
  SD <- apply(Data, 2, sd) %>%
    as.data.frame() %>%
    rownames_to_column(var = "node") %>%
    rename(sd = ".")

  # Unir los datos de centralidad y SD
  combined_data <- inner_join(Centralitys$table, SD, by = "node")

  # Seleccionar las columnas 3, 4 y 5 dinámicamente
  column3 <- colnames(combined_data)[3]
  column4 <- colnames(combined_data)[4]
  column5 <- colnames(combined_data)[5]

  # Calcular la correlación
  result <- combined_data %>%
    correlation::correlation(select = c(column3, column4), select2 = column5)

  return(result)
}
