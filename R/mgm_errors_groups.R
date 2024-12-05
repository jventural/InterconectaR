mgm_errors_groups <- function(data, type, level, group, columns) {
  library(mgm)
  library(tidyverse)
  # Selección de variables y agrupamiento
  mgm_group <- data %>%
    select(all_of(columns), {{ group }}) %>%
    group_nest({{ group }}) %>%
    mutate(
      data = map(data, ~ select(., tidyselect:::where(~ !all(is.na(.))))), # Eliminar columnas con todos los valores NA
      Estimacion = map(data, ~ mgm(as.matrix(.x),  # Convertir a matriz
                                   type = type,
                                   level = level))
    )

  # Predicción y almacenamiento
  predictive <- map2(mgm_group$Estimacion, mgm_group$data, ~
                       predict(object = .x,
                               data = .y,
                               errorCon = c("RMSE", "R2"),
                               errorCat = c("CC", "nCC", "CCmarg")))

  # Cálculo de errores
  error <- map(predictive, ~ .x$errors %>%
                 as_tibble() %>%
                 select(RMSE, R2, CC, nCC))

  # Nombrar los errores con los valores de 'group'
  names(error) <- mgm_group %>% pull({{ group }})

  return(error)
}
