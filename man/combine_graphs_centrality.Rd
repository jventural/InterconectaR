\name{combine_graphs_centrality}
\alias{combine_graphs_centrality}
\title{Combine Centrality and Comparative Plots into a Unified Visualization}

\description{
The \code{combine_graphs_centrality} function creates a composite visualization by combining a centrality network graph (generated using the \code{qgraph} package) with a comparative \code{ggplot2} visualization. It is designed for flexibility and supports publication-quality output with customizable layouts and high-resolution saving options.

\strong{Key Features}:
\itemize{
  \item Integrates centrality analysis with comparative visualizations for a holistic view of network data.
  \item Automatically handles the installation and loading of required packages.
  \item Supports customization of \code{qgraph} properties, including node grouping, labels, edge visualization, and layout options.
  \item Provides flexible layout adjustments for the combined visualization (e.g., column count and plot widths).
  \item Saves high-resolution outputs suitable for publications or presentations.
}
}

\usage{
combine_graphs_centrality(Figura1_Derecha, network, groups, error_Model,
                          labels = NULL, ncol = 2, widths = c(0.50, 0.60),
                          output_path = "Output/Figura_1_Final.jpg",
                          show_plot = FALSE, dpi = 600, legend.cex = 0.1)
}

\arguments{
\item{Figura1_Derecha}{A \code{ggplot2} object representing the comparative plot to be integrated into the final visualization.}
\item{network}{A list containing the network structure for the centrality graph. The \code{graph} component must be a matrix suitable for \code{qgraph}.}
\item{groups}{A vector or list specifying the grouping of nodes for coloring in the centrality graph.}
\item{error_Model}{A vector of numerical values (e.g., proportions or scores) for pie chart visualizations on the nodes in the \code{qgraph} graph.}
\item{labels}{Optional. A vector of custom labels for the nodes in the centrality graph. Defaults to \code{network$labels} if not specified.}
\item{ncol}{An integer indicating the number of columns in the combined visualization layout. Default is \code{2}.}
\item{widths}{A numeric vector specifying the relative widths of the two plots in the layout. Default is \code{c(0.50, 0.60)}.}
\item{output_path}{A string specifying the file path where the final visualization will be saved as a high-resolution JPEG image. Default is \code{"Output/Figura_1_Final.jpg"}.}
\item{show_plot}{Logical. If \code{TRUE}, the combined visualization is displayed in the RStudio plot viewer. If \code{FALSE}, it is saved to the file specified in \code{output_path}. Default is \code{FALSE}.}
\item{dpi}{An integer specifying the resolution (dots per inch) for the saved visualization. Default is \code{600}.}
\item{legend.cex}{A numeric value controlling the size of the legend in the centrality graph. Default is \code{0.1}.}
}

\details{
The function generates the centrality graph using \code{qgraph} and saves it as a temporary PNG file with the specified resolution. The saved image is then imported into R as a raster object using the \code{png} package. This rasterized graph is combined with the \code{ggplot2} visualization (\code{Figura1_Derecha}) using \code{gridExtra} for layout management.

The output can be displayed interactively (\code{show_plot = TRUE}) or saved to a file for further use (\code{show_plot = FALSE}). Temporary files created during the process are automatically deleted after execution.

The centrality graph is highly customizable, with options for node sizes, edge visualization, pie charts based on \code{error_Model}, and layout adjustments.
}

\value{
The function does not return an object. Instead, it produces:
\itemize{
  \item A displayed plot in the RStudio plot viewer (\code{show_plot = TRUE}).
  \item A high-resolution JPEG file saved at the location specified by \code{output_path} (\code{show_plot = FALSE}).
}
}

\examples{
# Example: Simulated network and comparative plot
network <- list(graph = matrix(sample(0:1, 100, replace = TRUE), 10, 10), labels = LETTERS[1:10])
groups <- rep(1:2, each = 5)
error_Model <- runif(10, 0, 1)

# Creating a ggplot2 object
library(ggplot2)
Figura1_Derecha <- ggplot(data.frame(x = 1:10, y = rnorm(10)), aes(x, y)) +
  geom_point() + theme_minimal()

# Save visualization
combine_graphs_centrality(Figura1_Derecha = Figura1_Derecha,
                          network = network,
                          groups = groups,
                          error_Model = error_Model,
                          ncol = 2,
                          widths = c(0.50, 0.60),
                          output_path = "Output/Figura_1_Final.jpg",
                          show_plot = FALSE)

# Show visualization
combine_graphs_centrality(Figura1_Derecha = Figura1_Derecha,
                          network = network,
                          groups = groups,
                          error_Model = error_Model,
                          show_plot = TRUE)
}

\author{
Dr. José Ventura-León
}
