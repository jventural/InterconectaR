\name{plot_centrality_stability}
\alias{plot_centrality_stability}
\title{Plot and Combine Centrality Stability and Non-Parametric Bootstrapping Results}

\description{
This function generates two plots: one for centrality stability metrics and another for non-parametric bootstrapping results. It combines the two plots into a single layout and saves the visualization as a high-resolution image.

\strong{Note}: This function automatically installs and loads the required libraries if they are not already available. The required libraries are \code{ggplot2}, \code{ggpubr}, and \code{scales}.
}

\usage{
plot_centrality_stability(caseDroppingBoot, nonParametricBoot,
statistics = "bridgeStrength",
output_path = "Output/Figura_2.jpg",
height = 5, width = 9, dpi = 600,
labels = FALSE)
}

\arguments{
\item{caseDroppingBoot}{An object containing the results of a case-dropping bootstrap analysis. This is used to create the first plot showing centrality stability metrics.}
\item{nonParametricBoot}{An object containing non-parametric bootstrapping results. This is used to create the second plot showing bootstrapped edge statistics.}
\item{statistics}{A character string specifying the centrality metric to plot for \code{caseDroppingBoot}. Default is \code{"bridgeStrength"}.}
\item{output_path}{A character string specifying the file path and name for the output image. Default is \code{"Output/Figura_2.jpg"}.}
\item{height}{Numeric. The height of the output image in inches. Default is \code{5}.}
\item{width}{Numeric. The width of the output image in inches. Default is \code{9}.}
\item{dpi}{Numeric. The resolution of the output image in dots per inch (DPI). Default is \code{600}.}
\item{labels}{Logical. Whether to include labels on the plots. Default is \code{FALSE}.}
}

\details{
The function performs the following steps:
\itemize{
\item Generates a plot (\code{p1}) for centrality stability metrics using \code{caseDroppingBoot} and the specified \code{statistics}.
\item Generates a second plot (\code{p2}) for bootstrapped edge statistics using \code{nonParametricBoot}.
\item Combines the two plots into a single layout using \code{ggpubr::ggarrange()}.
\item Saves the combined plot as a high-resolution JPEG image at the specified \code{output_path}.
}

This function is particularly useful for visualizing the stability and variability of network metrics, making it easy to compare results from different methods.
}

\value{
The function returns the combined plot object as a \code{ggpubr::ggarrange} object, allowing for further customization or inspection in R. The output is also saved as a JPEG file.
}

\examples{
Generate and save the combined plot
combined_plot <- plot_centrality_stability(
caseDroppingBoot = caseDroppingBoot,
nonParametricBoot = nonParametricBoot,
statistics = "bridgeStrength",
output_path = "Output/Figura_2_Final.jpg",
height = 5,
width = 9,
dpi = 600,
labels = FALSE
)

Inspect the combined plot
print(combined_plot)
}

\author{
Dr. José Ventura-León
}

