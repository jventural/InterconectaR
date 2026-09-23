#' @title Bootstrap and Evaluate Network Analysis
#' @description Performs bootstrap evaluation of network analysis algorithms.
#' @param data Data frame with the data.
#' @param true_network Reference network used as the truth: an object returned
#'   by `EGAnet::EGA()` (its `network` element is compared with each estimate).
#' @param algorithms Vector of algorithm names to evaluate.
#' @param correlation_methods Vector of correlation methods.
#' @param sample_sizes Vector of sample sizes to test (default c(100, 250, 500, 1000)).
#' @param n_simulations Number of simulations (default 100).
#' @param seed Optional random seed for reproducible results. `NULL` (default)
#'   leaves the random number generator untouched.
#' @param n_cores Number of cores for parallel processing (default 2). With
#'   `n_cores = 1` the simulations run sequentially.
#' @return Data frame with performance metrics.
#' @export
#' @importFrom EGAnet EGA
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#' @importFrom dplyr bind_rows
#' @examples
#' \donttest{
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
#' true_net <- EGAnet::EGA(items, plot.EGA = FALSE)
#' res <- boot_and_evaluate(items, true_network = true_net,
#'                          algorithms = "walktrap",
#'                          correlation_methods = "pearson",
#'                          sample_sizes = c(100, 200),
#'                          n_simulations = 2, seed = 1, n_cores = 1)
#' head(res)
#' }
boot_and_evaluate <- function(data, true_network, algorithms, correlation_methods,
                              sample_sizes = c(100, 250, 500, 1000), n_simulations = 100,
                              seed = NULL, n_cores = 2) {

  # Configura el entorno paralelo y restaura el plan del usuario al salir
  old_plan <- if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(old_plan), add = TRUE)

  # Funciones auxiliares para calcular la correlacion y el sesgo
  cor0 <- function(matrix1, matrix2, ...) {
    vector1 <- as.vector(matrix1)
    vector2 <- as.vector(matrix2)
    if (sum(!is.na(vector1)) < 2 || sum(!is.na(vector2)) < 2 || stats::sd(vector1, na.rm = TRUE) == 0 || stats::sd(vector2, na.rm = TRUE) == 0) {
      return(0)
    } else {
      return(stats::cor(vector1, vector2, ...))
    }
  }

  bias <- function(x, y) mean(abs(x - y), na.rm = TRUE)

  comparison_metrics <- function(real, est, name = "comparison") {
    TruePos <- sum(est != 0 & real != 0)
    FalsePos <- sum(est != 0 & real == 0)
    TrueNeg <- sum(est == 0 & real == 0)
    FalseNeg <- sum(est == 0 & real != 0)
    FDR <- FalsePos / (TruePos + FalsePos)
    out <- list(
      sensitivity = TruePos / (TruePos + FalseNeg),
      specificity = TrueNeg / (TrueNeg + FalsePos),
      precision = TruePos / (TruePos + FalsePos),
      FDR = FDR,
      correlation = cor0(est, real),
      abs_cor = cor0(abs(est), abs(real)),
      bias = bias(est, real),
      truePos = TruePos,
      falsePos = FalsePos,
      trueNeg = TrueNeg,
      falseNeg = FalseNeg
    )
    if (name != "") {
      names(out) <- paste0(names(out), "_", name)
    }
    out
  }

  performance <- function(real_network, estimated_network) {
    results <- comparison_metrics(real = as.matrix(real_network$network), est = as.matrix(estimated_network$network))
    results_df <- as.data.frame(t(unlist(results)))
    results_df <- round(results_df, 2)
    selected_columns <- c("sensitivity_comparison", "specificity_comparison", "precision_comparison",
                          "correlation_comparison", "abs_cor_comparison", "bias_comparison", "FDR_comparison")
    results_df <- results_df[, selected_columns]
    names(results_df) <- gsub("_comparison", "", names(results_df))
    return(results_df)
  }

  if (!is.null(seed)) set.seed(seed)
  results_list <- list()

  progressr::with_progress({
    p <- progressr::progressor(along = 1:n_simulations)
    results_list <- future.apply::future_lapply(1:n_simulations, function(simulation) {
      p()
      simulation_results <- list()
      for (sample_size in sample_sizes) {
        sampled_data <- data[sample(nrow(data), size = ceiling(sample_size / nrow(data) * nrow(data)), replace = TRUE), ]
        for (algorithm in algorithms) {
          for (cor_method in correlation_methods) {
            tryCatch({
              if (algorithm == "leiden") {
                ega_result <- EGAnet::EGA(data = sampled_data, algorithm = algorithm, corr = cor_method,
                                objective_function = "CPM", resolution_parameter = 0.05,
                                plot.EGA = FALSE)
              } else {
                ega_result <- EGAnet::EGA(data = sampled_data, algorithm = algorithm, corr = cor_method, plot.EGA = FALSE)
              }
              if (!is.null(ega_result)) {
                performance_results <- performance(true_network, ega_result)
                performance_results$Algorithm <- algorithm
                performance_results$Correlation_Method <- cor_method
                performance_results$Simulation <- simulation
                performance_results$Sample_Size <- sample_size
                performance_results$TEFI <- ega_result$TEFI
                simulation_results[[paste(algorithm, cor_method, sample_size, simulation)]] <- performance_results
              }
            }, error = function(e) {
              message("Error with ", algorithm, " using ", cor_method, " in simulation ", simulation, " with sample size ", sample_size, ": ", e$message)
            })
          }
        }
      }
      dplyr::bind_rows(simulation_results, .id = "Model_ID")
    }, future.seed = TRUE)
  })

  results_df <- dplyr::bind_rows(results_list)
  return(results_df)
}
