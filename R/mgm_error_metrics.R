#' MGM Error Metrics
#'
#' Fits a Mixed Graphical Model and extracts prediction error metrics.
#'
#' @param data Data frame or matrix with the data.
#' @param type Character vector of variable types ("g", "c", "p").
#' @param level Integer vector of variable levels.
#' @param k Order of interactions (default: 2).
#'
#' @export
#' @importFrom mgm mgm
#' @importFrom dplyr %>%
mgm_error_metrics <- function(data, type, level, k = 2) {
  # Validar que type y level tengan el mismo largo que las columnas de la data
  if (length(type) != ncol(data) | length(level) != ncol(data)) {
    stop("Los argumentos 'type' y 'level' deben tener la misma longitud que las columnas de 'data'.")
  }

  # =========================================================================
  # VALIDACION DE DATOS
  # =========================================================================

  # Convertir data a matriz
  data_matrix <- data %>% as.matrix()

  # Verificar valores NA

  na_count <- sum(is.na(data_matrix))
  if (na_count > 0) {
    na_by_col <- colSums(is.na(data_matrix))
    cols_with_na <- names(na_by_col[na_by_col > 0])
    stop(paste0("Se encontraron ", na_count, " valores NA en los datos. ",
                "Columnas afectadas: ", paste(cols_with_na, collapse = ", "), ". ",
                "mgm no permite valores faltantes. Por favor, elimine o impute los NA."))
  }

  # Verificar valores infinitos
  inf_count <- sum(is.infinite(data_matrix))
  if (inf_count > 0) {
    stop(paste0("Se encontraron ", inf_count, " valores infinitos en los datos. ",
                "mgm no permite valores Inf/-Inf."))
  }

  # Verificar consistencia de type y level para variables categoricas
  for (i in seq_along(type)) {
    if (type[i] == "c") {
      unique_vals <- length(unique(data_matrix[, i]))
      if (level[i] < unique_vals) {
        warning(paste0("Variable ", i, " (", colnames(data_matrix)[i], "): ",
                       "level=", level[i], " pero tiene ", unique_vals, " valores \u00fanicos. ",
                       "Considere ajustar level=", unique_vals))
      }
    }
  }

  # =========================================================================
  # AJUSTE DEL MODELO MGM
  # =========================================================================

  # Ajustar modelo MGM con el valor de k especificado
  mgm_model <- mgm::mgm(
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

  # =========================================================================
  # EXTRAER ERRORES SEGUN TIPO DE VARIABLE
  # =========================================================================

  # Identificar indices de variables continuas y categoricas
  idx_continuous <- which(type == "g" | type == "p")  # gaussiano o poisson
  idx_categorical <- which(type == "c")               # categorico

  # Crear dataframe de errores con nombres de variables
  var_names <- colnames(data_matrix)
  if (is.null(var_names)) {
    var_names <- paste0("V", 1:ncol(data_matrix))
  }

  # Construir resultado segun los tipos de variables presentes
  errors <- list()

  # Si hay variables continuas, extraer errorCon
  if (length(idx_continuous) > 0) {
    errors$errorCon <- data.frame(
      Variable = var_names[idx_continuous],
      RMSE = pred_model$errors$RMSE[idx_continuous],
      R2 = pred_model$errors$R2[idx_continuous],
      row.names = NULL
    )
  }

  # Si hay variables categoricas, extraer errorCat
  if (length(idx_categorical) > 0) {
    errors$errorCat <- data.frame(
      Variable = var_names[idx_categorical],
      CC = pred_model$errors$CC[idx_categorical],
      nCC = pred_model$errors$nCC[idx_categorical],
      row.names = NULL
    )
  }

  # Si todas las variables son del mismo tipo, tambien devolver vector R2 o nCC para compatibilidad
  if (length(idx_continuous) == ncol(data_matrix)) {
    errors$R2 <- pred_model$errors$R2
    names(errors$R2) <- var_names
  } else if (length(idx_categorical) == ncol(data_matrix)) {
    errors$nCC <- pred_model$errors$nCC
    names(errors$nCC) <- var_names
  }

  return(errors)
}
