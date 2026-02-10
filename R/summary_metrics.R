#' @title Summarize Network Performance Metrics
#' @description Calculates median values of network performance metrics
#'   grouped by algorithm, correlation method, and sample size.
#' @param results Data frame from boot_and_evaluate with raw metrics.
#' @return A data frame with averaged metrics.
#' @export
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
