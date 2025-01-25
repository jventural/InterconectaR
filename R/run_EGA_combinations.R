run_EGA_combinations <- function(data,
                                 corr = c("cor_auto", "pearson", "spearman"),
                                 algorithm = c("leiden", "louvain", "walktrap"),
                                 leiden_args = list(objective_function = "CPM",
                                                    resolution_parameter = 1.0)) {

  # Validar parámetros de entrada
  corr <- match.arg(corr, several.ok = TRUE)
  algorithm <- match.arg(algorithm, several.ok = TRUE)

  # Crear combinaciones posibles
  combinations <- expand.grid(corr = corr, algorithm = algorithm, stringsAsFactors = FALSE)

  # Lista para resultados
  results <- list()

  # Iterar sobre combinaciones
  for(i in seq_len(nrow(combinations))) {
    current_corr <- combinations$corr[i]
    current_alg <- combinations$algorithm[i]

    # Configurar parámetros específicos para Leiden
    if(current_alg == "leiden") {
      args <- list(
        data = data,
        corr = current_corr,
        model = "glasso",
        algorithm = current_alg,
        plot.EGA = FALSE
      )
      args <- modifyList(args, leiden_args)
    } else {
      args <- list(
        data = data,
        corr = current_corr,
        model = "glasso",
        algorithm = current_alg,
        plot.EGA = FALSE
      )
    }

    # Nombre único para la combinación
    combo_name <- paste(current_corr, current_alg, sep = ".")

    # Ejecutar con manejo de errores y advertencias
    results[[combo_name]] <- tryCatch({
      suppressWarnings(do.call(EGA, args))
    }, error = function(e) {
      message("Error en ", combo_name, ": ", e$message)
      return(NULL)
    })
  }

  # Filtrar resultados nulos
  results <- results[!sapply(results, is.null)]

  # Agregar metadatos
  attr(results, "combinations") <- combinations
  attr(results, "leiden_args") <- leiden_args
  class(results) <- c("EGA_combinations", class(results))

  return(results)
}
