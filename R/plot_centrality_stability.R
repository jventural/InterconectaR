#' Plot centrality stability diagnostics
#'
#' @param caseDroppingBoot Case-dropping bootstrap result object.
#' @param nonParametricBoot Non-parametric bootstrap result object.
#' @param statistics Character string specifying the statistic (default: "bridgeStrength").
#' @param labels Logical; whether to show labels (default: FALSE).
#' @param color_palette Optional character vector of colors for the statistics.
#'   If NULL (default), uses bootnet's default colors. Length should match
#'   the number of statistics being plotted.
#'
#' @return A combined patchwork plot object.
#' @export
#' @importFrom ggplot2 scale_y_continuous scale_color_manual scale_fill_manual
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom scales number_format
plot_centrality_stability <- function(caseDroppingBoot, nonParametricBoot,
                                       statistics = "bridgeStrength",
                                       labels = FALSE,
                                       color_palette = NULL) {

  # 1) Crear p1 y anadir scale_y sin importar si ya existe--luego lo silenciaremos al imprimir
  p1 <- plot(caseDroppingBoot, statistics = statistics, labels = labels) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.10),
      labels = scales::number_format(accuracy = 0.01)
    )

  # Override colores si se proporcionan
  if (!is.null(color_palette)) {
    stat_names <- statistics
    pal <- stats::setNames(
      rep_len(color_palette, length(stat_names)),
      stat_names
    )
    p1 <- p1 +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::scale_fill_manual(values = pal)
  }

  # 2) Crear p2
  p2 <- plot(nonParametricBoot, labels = labels, order = "sample", statistics = "edge")

  # 3) Combinar usando patchwork
  combinado <- (p1 + p2) +
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(tag_levels = "A")

  # 4) Asignar clase personalizada y definir print.silent para suprimir ese warning al imprimir
  class(combinado) <- c("silent_plot", class(combinado))
  assign("print.silent_plot",
         function(x, ...) suppressWarnings(NextMethod()),
         envir = .GlobalEnv)

  # 5) Devolver el objeto combinado
  return(combinado)
}
