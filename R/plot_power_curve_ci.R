#' Plot the Power Curve with Bootstrap Confidence Band
#'
#' @description
#' Visualizes the isotonic power curve produced by
#' \code{sample_size_EGA_montecarlo()} together with a 95 percent bootstrap
#' band, the recommended sample size and its confidence interval as a
#' bracket on the x axis, and (optionally) the empirical validation point.
#'
#' Conceptually related to the bootstrap-spline plot in the powerly package
#' (Constantin et al., 2023), but distinct in styling, color palette,
#' annotations (bracket on the x axis, validation marker) and underlying
#' object (combined power, not an arbitrary statistic).
#'
#' @param x Object returned by \code{sample_size_EGA_montecarlo()} run with
#'   \code{recommendation_method = "interpolated"}.
#' @param show_validation Logical; overlay the empirical power at the
#'   validation \code{n} (when \code{validation_n_rep > 0} was used).
#' @param show_observed Logical; overlay the observed (non-bootstrapped)
#'   power per grid point.
#' @param ribbon_alpha Numeric in (0, 1); transparency of the bootstrap band.
#' @param palette Character vector of length 3 with colors for: median curve,
#'   bootstrap band, validation/recommendation accents.
#' @param ... Ignored. For \code{plot()} compatibility.
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
#' plot_power_curve_ci(res)
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point geom_hline geom_vline geom_segment scale_color_manual scale_fill_manual labs theme_minimal theme element_text annotate
plot_power_curve_ci <- function(
    x,
    show_validation = TRUE,
    show_observed   = TRUE,
    ribbon_alpha    = 0.25,
    palette         = c(median = "#2A6F4D",
                        ribbon = "#7FB99B",
                        accent = "#C25E2A"),
    ...
) {
  if (!inherits(x, "sample_size_EGA_montecarlo")) {
    stop("'x' must be an object returned by sample_size_EGA_montecarlo().")
  }
  if (is.null(x$bootstrap_power_ci)) {
    stop("Bootstrap power band not available. Re-run with ",
         "recommendation_method = \"interpolated\".")
  }

  band <- x$bootstrap_power_ci
  target <- x$interpolation$target
  n_star <- x$recommended_n_interpolated
  ci     <- x$recommended_n_ci

  observed <- data.frame(
    n     = x$summary$Sample_Size,
    power = x$summary$power_combined
  )

  validation <- NULL
  if (isTRUE(show_validation) && !is.null(x$validation)) {
    validation <- data.frame(
      n     = x$validation$n,
      power = x$validation$power_combined
    )
  }

  # Build CI bracket data on the x axis (drawn at y_bracket).
  y_bracket <- -0.04
  bracket_df <- if (!is.null(ci) && all(is.finite(ci))) {
    data.frame(
      x_start = c(ci["2.5%"], ci["2.5%"], ci["97.5%"]),
      x_end   = c(ci["97.5%"], ci["2.5%"], ci["97.5%"]),
      y_start = c(y_bracket, y_bracket - 0.015, y_bracket - 0.015),
      y_end   = c(y_bracket, y_bracket + 0.015, y_bracket + 0.015)
    )
  } else {
    NULL
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data    = band,
      mapping = ggplot2::aes(x = .data$x, ymin = .data$q025, ymax = .data$q975),
      fill    = palette["ribbon"],
      alpha   = ribbon_alpha
    ) +
    ggplot2::geom_line(
      data      = band,
      mapping   = ggplot2::aes(x = .data$x, y = .data$median),
      color     = palette["median"],
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = target,
      linetype   = "dashed",
      color      = "grey30"
    ) +
    ggplot2::geom_vline(
      xintercept = n_star,
      linetype   = "dotted",
      color      = palette["accent"],
      linewidth  = 0.6
    )

  if (isTRUE(show_observed)) {
    p <- p + ggplot2::geom_point(
      data    = observed,
      mapping = ggplot2::aes(x = .data$n, y = .data$power),
      color   = palette["median"],
      size    = 2,
      shape   = 21,
      fill    = "white",
      stroke  = 1
    )
  }

  if (!is.null(bracket_df)) {
    p <- p + ggplot2::geom_segment(
      data    = bracket_df,
      mapping = ggplot2::aes(x = .data$x_start, xend = .data$x_end,
                             y = .data$y_start, yend = .data$y_end),
      color     = palette["accent"],
      linewidth = 0.7
    )
  }

  if (!is.null(validation)) {
    p <- p + ggplot2::geom_point(
      data    = validation,
      mapping = ggplot2::aes(x = .data$n, y = .data$power),
      color   = palette["accent"],
      fill    = palette["accent"],
      size    = 3.6,
      shape   = 23
    )
  }

  subtitle <- if (!is.null(ci) && all(is.finite(ci))) {
    sprintf("N* = %g  |  IC 95%% = [%g, %g]  |  power_target = %.2f",
            n_star, ci["2.5%"], ci["97.5%"], target)
  } else {
    sprintf("N* = %g  |  power_target = %.2f", n_star, target)
  }

  caption <- if (!is.null(validation)) {
    sprintf("Romboide: power empirico en validacion (n = %g, %d reps) = %.3f",
            validation$n, x$validation$n_rep, validation$power)
  } else {
    NULL
  }

  p +
    ggplot2::labs(
      title    = "Curva de power con banda bootstrap (95%)",
      subtitle = subtitle,
      x        = "Tamano muestral",
      y        = "Power combinado (Pr cumple todos los targets)",
      caption  = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      plot.caption  = ggplot2::element_text(hjust = 0.5, size = 9, color = "grey25")
    )
}
