#' Plot Replication-Level Failure Decomposition Per Sample Size
#'
#' @description
#' Stacked-bar visualization showing what fraction of Monte Carlo replications
#' fails each constraint at every sample size. The bars decompose
#' \code{1 - power_combined} into mutually exclusive failure modes (in
#' priority order): non-convergence, incorrect dimension recovery, and one
#' bar per target metric whose threshold was not met. The complementary
#' green segment shows the success rate (\code{power_combined}).
#'
#' This is an original visualization (not present in powerly) intended to
#' diagnose which constraint is the bottleneck at low \code{n}: e.g., is
#' it convergence, dimension recovery, or a specific recovery target?
#'
#' @param x Object returned by \code{sample_size_EGA_montecarlo()}.
#' @param order_targets Optional character vector reordering the failure-mode
#'   priorities for the metric targets. The default order is
#'   \code{convergence -> dimensions -> targets in the order they appear in
#'   x$targets -> success}.
#' @param ... Ignored.
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
#' plot_failure_decomposition(res)
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual labs theme_minimal theme element_text
#' @importFrom dplyr %>% group_by summarise n
plot_failure_decomposition <- function(x, order_targets = NULL, ...) {
  if (!inherits(x, "sample_size_EGA_montecarlo")) {
    stop("'x' must be an object returned by sample_size_EGA_montecarlo().")
  }
  if (is.null(x$raw) || nrow(x$raw) == 0) {
    stop("No raw replications available in 'x$raw'.")
  }

  raw <- x$raw
  targets <- x$targets

  metric_order <- if (!is.null(order_targets)) {
    intersect(order_targets, names(targets))
  } else {
    names(targets)
  }

  # Per-replication failure category in priority order:
  #   1) failed_convergence  (most fundamental)
  #   2) failed_dimensions   (correct K is the second gate)
  #   3) failed_<metric>     (one per target, in user-specified order)
  #   4) success
  raw$failure_category <- "success"
  raw$failure_category[!is.na(raw$converged)        & !raw$converged]        <- "failed_convergence"
  failed_so_far <- raw$failure_category != "success"

  if ("dimensions_correct" %in% names(raw)) {
    dim_failed <- !is.na(raw$dimensions_correct) & !raw$dimensions_correct
    needs_assign <- dim_failed & !failed_so_far
    raw$failure_category[needs_assign] <- "failed_dimensions"
    failed_so_far <- failed_so_far | needs_assign
    # Reps where dimensions_correct is NA but converged TRUE should not happen,
    # but if they do treat them as failed_dimensions.
    na_dim <- is.na(raw$dimensions_correct) &
              !is.na(raw$converged) & raw$converged & !failed_so_far
    raw$failure_category[na_dim] <- "failed_dimensions"
    failed_so_far <- failed_so_far | na_dim
  }

  for (metric in metric_order) {
    meets_col <- paste0("meets_", metric)
    if (!meets_col %in% names(raw)) next
    metric_failed <- !is.na(raw[[meets_col]]) & !raw[[meets_col]]
    needs_assign <- metric_failed & !failed_so_far
    raw$failure_category[needs_assign] <- paste0("failed_", metric)
    failed_so_far <- failed_so_far | needs_assign
  }

  fraction_table <- raw %>%
    dplyr::group_by(.data$Sample_Size, .data$failure_category) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$Sample_Size) %>%
    dplyr::mutate(fraction = .data$count / sum(.data$count)) %>%
    dplyr::ungroup()

  category_levels <- c("success", "failed_convergence", "failed_dimensions",
                       paste0("failed_", metric_order))
  category_levels <- intersect(category_levels, unique(fraction_table$failure_category))
  fraction_table$failure_category <- factor(fraction_table$failure_category,
                                            levels = rev(category_levels))

  category_colors <- c(
    success            = "#2A6F4D",
    failed_convergence = "#5C3A21",
    failed_dimensions  = "#A65A2C",
    failed_ari              = "#D49B5C",
    failed_edge_correlation = "#E3C46A",
    failed_sensitivity      = "#B79968",
    failed_specificity      = "#8C7A5C",
    failed_precision        = "#C97D60",
    failed_fdr              = "#7E5E4F"
  )
  fill_values <- category_colors[as.character(levels(fraction_table$failure_category))]
  fill_labels <- gsub("_", " ", levels(fraction_table$failure_category))

  ggplot2::ggplot(
    fraction_table,
    ggplot2::aes(x = factor(.data$Sample_Size), y = .data$fraction,
                 fill = .data$failure_category)
  ) +
    ggplot2::geom_col(width = 0.78, color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_manual(
      name   = "Categoria por replica",
      values = fill_values,
      labels = fill_labels,
      breaks = levels(fraction_table$failure_category),
      drop   = FALSE
    ) +
    ggplot2::labs(
      title    = "Descomposicion de fallos por restriccion",
      subtitle = "Cada barra suma 1; verde = exito, tonos calidos = restriccion violada",
      x        = "Tamano muestral",
      y        = "Fraccion de replicas"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    )
}
