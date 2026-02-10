#' @title Plot Network Performance Metrics
#' @description Creates faceted plots comparing performance metrics across
#'   algorithms and correlation methods.
#' @param averaged_results Data frame with averaged metrics from summary_metrics.
#' @return A ggplot object with performance comparison.
#' @export
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line labs facet_grid vars theme_minimal theme element_text scale_color_brewer
plot_performance_metrics <- function(averaged_results) {
  # Transformar los datos para ggplot
  data_long <- averaged_results %>%
    tidyr::pivot_longer(cols = avg_sensitivity:avg_bias, names_to = "Metric", values_to = "Value")

  # Crear el grafico
  plot <- ggplot2::ggplot(data_long, ggplot2::aes(x = Sample_Size, y = Value, color = Correlation_Method)) +
    ggplot2::geom_line(ggplot2::aes(linetype = Correlation_Method), size = 0.8) +
    ggplot2::labs(
      title = "Comparison of Performance Metrics by Algorithm and Correlation Method",
      x = "Sample Size",
      y = "Value"
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(Metric), rows = ggplot2::vars(Algorithm)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 12)
    ) +
    ggplot2::scale_color_brewer(palette = "Set1")

  # Retornar el objeto plot
  return(plot)
}
