#' @title Choose Best Method
#' @description Selects the best correlation comparison method based on sample size and average inter-correlation.
#' @param n Integer, sample size.
#' @param r_matrix Square correlation matrix.
#' @return A list with the best method name and details.
#' @export
choose_best_method <- function(n, r_matrix) {
  # Verifica si la matriz es valida
  if (!is.matrix(r_matrix) || nrow(r_matrix) != ncol(r_matrix)) {
    stop("La matriz de correlaci\u00f3n debe ser cuadrada.")
  }

  # Calcula intercorrelacion promedio entre variables comunes (opcional)
  r_mean <- mean(r_matrix[upper.tri(r_matrix)])

  # Criterios para elegir el mejor metodo
  if (n < 300) {
    if (r_mean > 0.10) {
      best_method <- "williams1959"  # Muestras pequenas, correlaciones bajas
    } else {
      best_method <- "hendrickson1970"  # Muestras pequenas, correlaciones altas
    }
  } else if (n >= 300 & n <= 500) {
    if (r_mean > 0.3) {
      best_method <- "steiger1980"  # Tamano moderado, correlaciones bajas
    } else {
      best_method <- "hittner2003"  # Tamano moderado, correlaciones altas
    }
  } else if (n > 500) {
    if (r_mean > 0.5) {
      best_method <- "hotelling1940"  # Muestras grandes, correlaciones moderadas
    } else {
      best_method <- "meng1992"  # Muestras grandes, correlaciones altas
    }
  }

  # Devuelve el mejor metodo
  return(list(
    best_method = best_method,
    details = list(
      n = n,
      r_mean = r_mean,
      recommendation = paste("El m\u00e9todo recomendado es:", best_method)
    )
  ))
}
