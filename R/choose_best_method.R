choose_best_method <- function(n, r_matrix) {
  # Verifica si la matriz es válida
  if (!is.matrix(r_matrix) || nrow(r_matrix) != ncol(r_matrix)) {
    stop("La matriz de correlación debe ser cuadrada.")
  }

  # Calcula intercorrelación promedio entre variables comunes (opcional)
  r_mean <- mean(r_matrix[upper.tri(r_matrix)])

  # Criterios para elegir el mejor método
  if (n < 300) {
    if (r_mean > 0.10) {
      best_method <- "williams1959"  # Muestras pequeñas, correlaciones bajas
    } else {
      best_method <- "hendrickson1970"  # Muestras pequeñas, correlaciones altas
    }
  } else if (n >= 300 & n <= 500) {
    if (r_mean > 0.3) {
      best_method <- "steiger1980"  # Tamaño moderado, correlaciones bajas
    } else {
      best_method <- "hittner2003"  # Tamaño moderado, correlaciones altas
    }
  } else if (n > 500) {
    if (r_mean > 0.5) {
      best_method <- "hotelling1940"  # Muestras grandes, correlaciones moderadas
    } else {
      best_method <- "meng1992"  # Muestras grandes, correlaciones altas
    }
  }

  # Devuelve el mejor método
  return(list(
    best_method = best_method,
    details = list(
      n = n,
      r_mean = r_mean,
      recommendation = paste("El método recomendado es:", best_method)
    )
  ))
}
