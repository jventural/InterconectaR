#' @title Summarize Network Performance Metrics
#' @description Calculates median values of network performance metrics
#'   grouped by algorithm, correlation method, and sample size.
#' @param results Data frame from boot_and_evaluate with raw metrics.
#' @return A data frame with averaged metrics.
#' @export
#' @examples
#' # A small table with the structure returned by boot_and_evaluate()
#' set.seed(1)
#' res <- expand.grid(Algorithm = c("louvain", "walktrap"),
#'                    Correlation_Method = c("pearson", "spearman"),
#'                    Sample_Size = c(100, 250, 500), Simulation = 1:5,
#'                    stringsAsFactors = FALSE)
#' k <- nrow(res)
#' res$sensitivity <- runif(k, 0.6, 0.9)
#' res$specificity <- runif(k, 0.7, 0.95)
#' res$precision <- runif(k, 0.6, 0.9)
#' res$correlation <- runif(k, 0.7, 0.95)
#' res$abs_cor <- runif(k, 0.7, 0.95)
#' res$bias <- runif(k, 0.01, 0.05)
#' res$TEFI <- rnorm(k, -5, 0.3)
#' res$FDR <- runif(k, 0.05, 0.2)
#'
#' summary_metrics(res)
#' @importFrom dplyr %>% group_by summarise
summary_metrics <- function(results) {
  # Calcular las medianas de las metricas agrupadas por Algorithm y Correlation_Method
  averaged_results <- results %>%
    dplyr::group_by(Algorithm, Correlation_Method, Sample_Size) %>%
    dplyr::summarise(
      avg_sensitivity = stats::median(sensitivity, na.rm = TRUE),
      avg_specificity = stats::median(specificity, na.rm = TRUE),
      avg_precision = stats::median(precision, na.rm = TRUE),
      avg_correlation = stats::median(correlation, na.rm = TRUE),
      avg_abs_cor = stats::median(abs_cor, na.rm = TRUE),
      avg_bias = stats::median(bias, na.rm = TRUE),
      avg_TEFI = stats::median(TEFI, na.rm = TRUE),
      avg_FDR = stats::median(FDR, na.rm = TRUE),
      .groups = 'drop'
    )
  return(averaged_results)
}
