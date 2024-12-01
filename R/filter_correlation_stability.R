filter_correlation_stability <- function(caseDroppingBoot) {
  # Verificar e instalar librerías necesarias
  required_packages <- c("dplyr", "tibble")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Calcular la estabilidad de las correlaciones
  CorStability <- corStability(caseDroppingBoot)

  # Convertir en data frame, redondear y filtrar
  result <- data.frame(Index = round(CorStability, 2)) %>%
    rownames_to_column() %>%
    filter(rowname %in% c("bridgeExpectedInfluence",
                          "bridgeStrength",
                          "expectedInfluence",
                          "strength"))

  return(result)
}
