#' @title Plot Average TEFI by Sample Size
#' @description Plots median TEFI values across different sample sizes,
#'   algorithms, and correlation methods.
#' @param results Data frame from boot_and_evaluate with TEFI values.
#' @return A ggplot object showing TEFI trends.
#' @export
#' @importFrom dplyr %>% group_by summarise
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous labs theme_minimal facet_wrap
plot_avg_TEFI <- function(results) {
  # Agrupar datos y calcular la mediana de TEFI para cada grupo
  summarised_results <- results %>%
    dplyr::group_by(Algorithm, Correlation_Method, Sample_Size) %>%
    dplyr::summarise(
      avg_TEFI = stats::median(TEFI, na.rm = TRUE),
      .groups = 'drop'
    )

  # Crear el grafico usando ggplot2
  plot <- ggplot2::ggplot(summarised_results, ggplot2::aes(x = Sample_Size, y = avg_TEFI, group = Algorithm, color = Algorithm, shape = Algorithm)) +
    ggplot2::geom_line() + # Lineas para conectar puntos
    ggplot2::geom_point() + # Puntos en cada dato
    ggplot2::scale_x_continuous(breaks = unique(summarised_results$Sample_Size), labels = unique(summarised_results$Sample_Size)) + # Ajustar las marcas y etiquetas del eje x
    ggplot2::labs(title = "Average TEFI by Sample Size",
         x = "Sample Size",
         y = "Average TEFI",
         color = "Algorithm") +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~Correlation_Method) # Organizar graficos por metodo de correlacion

  # Retornar el objeto plot
  return(plot)
}
