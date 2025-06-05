\name{plot_centrality_stability}
\alias{plot_centrality_stability}
\title{Plot and Combine Centrality Stability and Non-Parametric Bootstrapping Results into a ggplot Object}

\description{
This function generates two plots—one for centrality stability metrics and another for non-parametric bootstrapping results—and combines them into a single \code{ggplot2} layout. Instead of saving internally, it returns a single \code{ggplot} object (of class \code{"silent_plot"}) so that users can call \code{ggsave()} or further modify it.

\strong{Note}: Required libraries (\code{ggplot2}, \code{patchwork}, and \code{scales}) are installed and loaded automatically if missing.
}

\usage{
plot_centrality_stability(caseDroppingBoot, nonParametricBoot,
statistics = "bridgeStrength",
labels = FALSE)
}

\arguments{
\item{\code{caseDroppingBoot}}{An object containing the results of a case‐dropping bootstrap analysis. Used to create the first plot showing centrality stability metrics.}
\item{\code{nonParametricBoot}}{An object containing non‐parametric bootstrapping results. Used to create the second plot showing bootstrapped edge statistics.}
\item{\code{statistics}}{A character string specifying the centrality metric to plot for \code{caseDroppingBoot}. Default is \code{"bridgeStrength"}.}
\item{\code{labels}}{Logical. Whether to include node labels on the plots. Default is \code{FALSE}.}
}

\details{
When \code{plot_centrality_stability} is called, the function:
\itemize{
\item Ensures that \code{ggplot2}, \code{patchwork}, and \code{scales} are installed and loaded.
\item Creates \code{p1} by calling \code{plot(caseDroppingBoot, statistics = statistics, labels = labels)} and then adding a custom \code{scale_y_continuous()} range from
−
1
−1 to
1
1 with breaks every
0.10
0.10. Any “Scale for y is already present…” warning is suppressed.
\item Creates \code{p2} by calling \code{plot(nonParametricBoot, labels = labels, order = "sample", statistics = "edge")}, again suppressing scale‐related warnings.
\item Combines \code{p1} and \code{p2} side‐by‐side using \code{patchwork::plot_layout(ncol = 2)} and adds automatic tags “A” and “B” via \code{plot_annotation(tag_levels = "A")}, suppressing any residual warnings during combination.
\item Assigns class \code{"silent_plot"} to the resulting \code{ggplot} so that printing or saving with \code{ggsave()} will not display the “Scale for y is already present…” message.
}
Since the function returns a fully composed \code{ggplot} object, users can save it externally (e.g., \code{ggsave()}) or add additional annotations as needed.
}

\value{
A \code{ggplot} object (class \code{"silent_plot"}). When printed or saved, scale‐duplication warnings are suppressed.
}

\examples{
\dontrun{

# Suponiendo que caseDroppingBoot y nonParametricBoot ya están creados:
combined_plot <- plot_centrality_stability(
caseDroppingBoot = caseDroppingBoot,
nonParametricBoot = nonParametricBoot,
statistics = "bridgeStrength",
labels = FALSE
)

# Guardar externamente con ggsave (9" × 5" a 600 dpi)
ggsave(
filename = "Figura_2.png",
plot = combined_plot,
width = 9,
height = 5,
units = "in",
dpi = 600
)

# Inspeccionar en R
combined_plot
}
}

\author{
Dr. José Ventura-León
}
