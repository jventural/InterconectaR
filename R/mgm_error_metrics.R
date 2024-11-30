mgm_error_metrics <- function(data, type, level, k = 2) {
  # Requerir e instalar mgm si no está instalado
  if (!require("mgm", character.only = TRUE)) {
    install.packages("mgm", dependencies = TRUE)
    library(mgm, character.only = TRUE)
  }

  # Requerir e instalar dplyr si no está instalado
  if (!require("dplyr", character.only = TRUE)) {
    install.packages("dplyr", dependencies = TRUE)
    library(dplyr, character.only = TRUE)
  }

  # Validar que type y level tengan el mismo largo que las columnas de la data
  if (length(type) != ncol(data) | length(level) != ncol(data)) {
    stop("Los argumentos 'type' y 'level' deben tener la misma longitud que las columnas de 'data'.")
  }

  # Convertir data a matriz
  data_matrix <- data %>% as.matrix()

  # Ajustar modelo MGM con el valor de k especificado
  mgm_model <- mgm(
    data = data_matrix,
    type = type,
    levels = level,
    k = k
  )

  # Predecir modelo
  pred_model <- predict(
    object = mgm_model,
    data = data,
    errorCon = c("RMSE", "R2"),
    errorCat = c("CC", "nCC")
  )

  # Extraer errores como dataframes para facilitar el acceso
  errors <- list(
    errorCon = pred_model$errors %>%
      select(RMSE, R2) %>%
      as.data.frame() %>%
      setNames(c("RMSE", "R2")),
    errorCat = pred_model$errors %>%
      select(CC, nCC) %>%
      as.data.frame() %>%
      setNames(c("CC", "nCC"))
  )

  return(errors)
}
