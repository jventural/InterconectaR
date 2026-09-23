#' Plot Monte Carlo EGA Sample Size Results
#'
#' @description
#' Creates a visual summary of the output from
#' \code{sample_size_EGA_montecarlo()}. The plot shows selected performance
#' metrics across candidate sample sizes, target criteria, and the recommended
#' sample size when one is available. Optionally adds 95 percent Monte Carlo
#' uncertainty ribbons for ARI and edge correlation.
#'
#' @param x Object returned by \code{sample_size_EGA_montecarlo()}.
#' @param metrics Character vector with summary metrics to plot. Defaults to
#'   the main recovery metrics bounded between 0 and 1.
#' @param show_criteria Logical; draw horizontal target lines when criteria are
#'   available.
#' @param show_recommended Logical; draw a vertical line at the recommended
#'   sample size when available.
#' @param show_uncertainty Logical; draw 95 percent Monte Carlo bands when the
#'   relevant quantile columns are present in \code{x$summary}.
#' @param point_size Numeric point size.
#' @param line_width Numeric line width.
#' @param ... Ignored. Included for compatibility with \code{plot()}.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' res <- sample_size_EGA_montecarlo(
#'   community_sizes = c(4, 4), within_edge = 0.20,
#'   sample_sizes = c(100, 200, 400), n_rep = 10,
#'   data_type = "continuous", seed = 1, verbose = FALSE,
#'   control = list(boots = 100, corr = "pearson", algorithm = "walktrap")
#' )
#' plot_sample_size_EGA_montecarlo(res)
#' @importFrom dplyr %>% all_of filter if_else mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline geom_vline geom_ribbon facet_wrap labs scale_color_manual theme_minimal theme element_text
plot_sample_size_EGA_montecarlo <- function(
    x,
    metrics = c(
      "p_dimensions",
      "median_ari",
      "median_edge_correlation",
      "median_sensitivity",
      "median_specificity",
      "median_precision",
      "median_fdr"
    ),
    show_criteria = TRUE,
    show_recommended = TRUE,
    show_uncertainty = TRUE,
    point_size = 2.4,
    line_width = 0.8,
    ...
) {

  if (!inherits(x, "sample_size_EGA_montecarlo")) {
    stop("'x' must be an object returned by sample_size_EGA_montecarlo().")
  }

  summary <- x$summary
  available_metrics <- intersect(metrics, names(summary))
  if (length(available_metrics) == 0) {
    stop("None of the requested metrics are available in x$summary.")
  }

  metric_labels <- c(
    p_dimensions = "Correct dimensions",
    median_ari = "Community recovery (ARI)",
    median_edge_correlation = "Edge-weight correlation",
    median_sensitivity = "Sensitivity",
    median_specificity = "Specificity",
    median_precision = "Precision",
    median_fdr = "False discovery rate",
    median_bias = "Absolute bias",
    median_TEFI = "TEFI",
    convergence_rate = "Convergence rate"
  )

  uncertainty_map <- c(
    median_ari = "ari",
    median_edge_correlation = "edge_correlation"
  )

  plot_data <- summary %>%
    dplyr::select(.data$Sample_Size, .data$meets_criteria, dplyr::all_of(available_metrics)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(available_metrics),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric_label = dplyr::if_else(
        .data$metric %in% names(metric_labels),
        unname(metric_labels[.data$metric]),
        .data$metric
      ),
      criteria_status = dplyr::if_else(.data$meets_criteria, "Meets all criteria", "Does not meet all criteria")
    )

  criteria_data <- .InterconectaR_plot_criteria_data(x$criteria, available_metrics, metric_labels)

  ribbon_data <- .InterconectaR_plot_ribbon_data(summary, available_metrics, metric_labels, uncertainty_map)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Sample_Size, y = .data$value)
  )

  if (isTRUE(show_uncertainty) && nrow(ribbon_data) > 0) {
    plot <- plot +
      ggplot2::geom_ribbon(
        data = ribbon_data,
        ggplot2::aes(x = .data$Sample_Size, ymin = .data$ymin, ymax = .data$ymax),
        inherit.aes = FALSE,
        fill = "#1B9E77",
        alpha = 0.15
      )
  }

  plot <- plot +
    ggplot2::geom_line(linewidth = line_width, color = "grey35") +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$criteria_status),
      size = point_size
    ) +
    ggplot2::facet_wrap(~metric_label, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = c(
        "Meets all criteria" = "#1B9E77",
        "Does not meet all criteria" = "#D95F02"
      ),
      name = NULL
    ) +
    ggplot2::labs(
      title = "EGA Monte Carlo Sample Size Evaluation",
      subtitle = .InterconectaR_plot_subtitle(x$recommended_n),
      x = "Sample size",
      y = "Metric value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (isTRUE(show_criteria) && nrow(criteria_data) > 0) {
    plot <- plot +
      ggplot2::geom_hline(
        data = criteria_data,
        ggplot2::aes(yintercept = .data$threshold),
        inherit.aes = FALSE,
        linetype = "dashed",
        color = "#3366AA",
        linewidth = 0.5
      )
  }

  if (isTRUE(show_recommended) && is.finite(x$recommended_n)) {
    plot <- plot +
      ggplot2::geom_vline(
        xintercept = x$recommended_n,
        linetype = "dotted",
        color = "#111111",
        linewidth = 0.6
      )
  }

  plot
}

#' @export
plot.sample_size_EGA_montecarlo <- function(x, ...) {
  plot_sample_size_EGA_montecarlo(x, ...)
}

.InterconectaR_plot_criteria_data <- function(criteria, metrics, metric_labels) {
  if (is.null(criteria) || length(criteria) == 0) {
    return(data.frame())
  }

  criteria_map <- c(
    p_dimensions = "p_dimensions",
    ari = "median_ari",
    edge_correlation = "median_edge_correlation",
    sensitivity = "median_sensitivity",
    specificity = "median_specificity",
    precision = "median_precision",
    fdr = "median_fdr",
    convergence_rate = "convergence_rate"
  )

  available <- names(criteria_map)[criteria_map %in% metrics & names(criteria_map) %in% names(criteria)]
  if (length(available) == 0) {
    return(data.frame())
  }

  metric <- unname(criteria_map[available])
  data.frame(
    metric = metric,
    metric_label = ifelse(metric %in% names(metric_labels), unname(metric_labels[metric]), metric),
    threshold = as.numeric(criteria[available]),
    stringsAsFactors = FALSE
  )
}

.InterconectaR_plot_ribbon_data <- function(summary, metrics, metric_labels, uncertainty_map) {
  rows <- list()
  for (metric in metrics) {
    base_name <- uncertainty_map[metric]
    if (is.na(base_name)) next
    lower_col <- paste0("q025_", base_name)
    upper_col <- paste0("q975_", base_name)
    if (!all(c(lower_col, upper_col) %in% names(summary))) next
    rows[[metric]] <- data.frame(
      Sample_Size = summary$Sample_Size,
      metric = metric,
      metric_label = unname(metric_labels[metric]),
      ymin = summary[[lower_col]],
      ymax = summary[[upper_col]],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

.InterconectaR_plot_subtitle <- function(recommended_n) {
  if (is.finite(recommended_n)) {
    paste0("Recommended minimum n = ", recommended_n)
  } else {
    "No candidate sample size met all selected criteria"
  }
}
