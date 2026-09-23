#' Optimize Leiden Resolution
#'
#' Optimizes the resolution parameter for Leiden community detection.
#'
#' @param data Data frame with the data.
#' @param corr Correlation method (default: "cor_auto").
#' @param gamma_values Numeric vector of gamma resolution values to test.
#' @param objective_function Leiden objective function (default: "CPM").
#' @param verbose Logical; print progress messages.
#'
#' @return A list with `best_gamma` (the resolution with the lowest TEFI),
#'   `max_valid_gamma` (the largest resolution tested before TEFI became
#'   undefined), `best_model` (the EGA object for `best_gamma`),
#'   `all_results` (the EGA objects of the valid resolutions), `comparison`
#'   (a data frame with the TEFI and number of communities per resolution) and
#'   `optimization_plot` (a ggplot of TEFI against gamma).
#' @export
#' @examples
#' set.seed(123)
#' n <- 300
#' f1 <- rnorm(n)
#' f2 <- 0.3 * f1 + rnorm(n)
#' items <- data.frame(
#'   sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'   sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#' )
#' names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
#'
#' opt <- optimize_leiden_resolution(items, corr = "pearson",
#'                                   gamma_values = c(0.05, 0.1, 0.5),
#'                                   verbose = FALSE)
#' opt$comparison
#' @importFrom EGAnet EGA
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
optimize_leiden_resolution <- function(data,
                                       corr = "cor_auto",
                                       gamma_values = c(0.01, 0.05, 0.1, 0.5, 1.0),
                                       objective_function = "CPM",
                                       verbose = TRUE) {

  if (!is.data.frame(data)) {
    stop("Los datos deben ser un dataframe")
  }

  # Almacenar resultados
  results <- list()
  # NA (not 0) marks the resolutions that were never tested: after an early
  # break they must not look like valid TEFI values
  tefi_values <- rep(NA_real_, length(gamma_values))
  names(tefi_values) <- as.character(gamma_values)

  # Variable para determinar el gamma maximo con TEFI valido
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
      if (is.na(max_valid_gamma) && i > 1) {
        max_valid_gamma <- gamma_values[i - 1] # Guardar el ultimo gamma valido
      }
      if (verbose) message("TEFI no v\u00e1lido (NaN) para gamma = ", current_gamma)
      break
    }
  }

  # Si no se encontro un maximo valido
  if (is.na(max_valid_gamma)) {
    max_valid_gamma <- gamma_values[length(gamma_values)]
  }

  # Filtrar resultados validos
  valid_indices <- which(!is.nan(tefi_values) & !is.na(tefi_values))

  if (length(valid_indices) == 0) {
    stop("Ning\u00fan valor gamma produjo resultados v\u00e1lidos")
  }

  best_index <- which.min(tefi_values[valid_indices])
  best_gamma <- gamma_values[valid_indices][best_index]
  best_model <- results[[as.character(best_gamma)]]

  # 'results' only holds the resolutions that ran, so it is indexed by name
  valid_results <- results[as.character(gamma_values[valid_indices])]

  # Crear dataframe comparativo
  comparison_df <- data.frame(
    gamma = gamma_values[valid_indices],
    TEFI = tefi_values[valid_indices],
    n_communities = vapply(valid_results, function(x) if (!is.null(x)) as.numeric(x$n.dim) else NA_real_, numeric(1)),
    convergence = vapply(valid_results, function(x) !is.null(x), logical(1))
  )

  # Resultado final
  return(list(
    best_gamma = best_gamma,
    max_valid_gamma = max_valid_gamma,
    best_model = best_model,
    all_results = valid_results,
    comparison = comparison_df,
    optimization_plot = ggplot2::ggplot(comparison_df, ggplot2::aes(x = gamma, y = TEFI)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::geom_point(color = "firebrick", size = 3) +
      ggplot2::labs(title = "Optimizaci\u00f3n de Par\u00e1metro de Resoluci\u00f3n",
                    x = "Valor Gamma", y = "TEFI") +
      ggplot2::theme_minimal()
  ))
}
