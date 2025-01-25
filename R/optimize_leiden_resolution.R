optimize_leiden_resolution <- function(data,
                                       corr = "cor_auto",
                                       gamma_values = c(0.01, 0.05, 0.1, 0.5, 1.0),
                                       objective_function = "CPM",
                                       verbose = TRUE) {

  # Validación de parámetros
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop("Paquete EGAnet requerido. Instala con install.packages('EGAnet')")
  }

  if (!is.data.frame(data)) {
    stop("Los datos deben ser un dataframe")
  }

  # Almacenar resultados
  results <- list()
  tefi_values <- numeric(length(gamma_values))
  names(tefi_values) <- as.character(gamma_values)

  # Variable para determinar el gamma máximo con TEFI válido
  max_valid_gamma <- NA

  # Iterar sobre valores gamma
  for (i in seq_along(gamma_values)) {
    current_gamma <- gamma_values[i]

    if (verbose) message("\nProbando gamma = ", current_gamma)

    # Ejecutar EGA con manejo de errores
    ega_result <- tryCatch({
      EGAnet::EGA(
        data = data,
        corr = corr,
        model = "glasso",
        algorithm = "leiden",
        objective_function = objective_function,
        resolution_parameter = current_gamma,
        plot.EGA = FALSE
      )
    }, error = function(e) {
      if (verbose) message("Error con gamma = ", current_gamma, ": ", e$message)
      return(NULL)
    })

    # Almacenar resultados
    if (!is.null(ega_result)) {
      results[[as.character(current_gamma)]] <- ega_result
      tefi_values[i] <- if (!is.null(ega_result$TEFI)) ega_result$TEFI else NA
    } else {
      tefi_values[i] <- NA
    }

    # Verificar si el TEFI es NaN
    if (is.nan(tefi_values[i])) {
      if (is.na(max_valid_gamma)) {
        max_valid_gamma <- gamma_values[i - 1] # Guardar el último gamma válido
      }
      if (verbose) message("TEFI no válido (NaN) para gamma = ", current_gamma)
      break
    }
  }

  # Si no se encontró un máximo válido
  if (is.na(max_valid_gamma)) {
    max_valid_gamma <- gamma_values[length(gamma_values)]
  }

  # Filtrar resultados válidos
  valid_indices <- which(!is.nan(tefi_values) & !is.na(tefi_values))

  if (length(valid_indices) == 0) {
    stop("Ningún valor gamma produjo resultados válidos")
  }

  best_index <- which.min(tefi_values[valid_indices])
  best_gamma <- gamma_values[valid_indices][best_index]
  best_model <- results[[as.character(best_gamma)]]

  # Crear dataframe comparativo
  comparison_df <- data.frame(
    gamma = gamma_values[valid_indices],
    TEFI = tefi_values[valid_indices],
    n_communities = sapply(results[valid_indices], function(x) if (!is.null(x)) x$n.dim else NA),
    convergence = sapply(results[valid_indices], function(x) !is.null(x))
  )

  # Resultado final
  return(list(
    best_gamma = best_gamma,
    max_valid_gamma = max_valid_gamma,
    best_model = best_model,
    all_results = results[valid_indices],
    comparison = comparison_df,
    optimization_plot = ggplot2::ggplot(comparison_df, ggplot2::aes(x = gamma, y = TEFI)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::geom_point(color = "firebrick", size = 3) +
      ggplot2::labs(title = "Optimización de Parámetro de Resolución",
                    x = "Valor Gamma", y = "TEFI") +
      ggplot2::theme_minimal()
  ))
}
