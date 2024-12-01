\name{combine_graphs_centrality}
\alias{combine_graphs_centrality}
\title{Combine Network Centrality and Comparative Plot into a Single Visualization}

\description{
This function combines a centrality graph generated using the \code{qgraph} package with a comparative plot (e.g., a ggplot object). The combined visualization can be saved as a high-resolution image or displayed directly in the RStudio plot window.

\strong{Note}: This function automatically installs and loads the required libraries if they are not already available. The required libraries are \code{ggplot2}, \code{qgraph}, \code{gridExtra}, \code{png}, \code{grid}, and \code{Cairo}.
}

\usage{
combine_graphs_centrality(Figura1_Derecha, network, groups, error_Model,
ncol = 2, widths = c(0.50, 0.60),
output_path = "Output/Figura_1_Final.jpg",
show_plot = FALSE)
}

\arguments{
\item{Figura1_Derecha}{A \code{ggplot2} object representing a comparative plot to include in the final visualization.}
\item{network}{An object containing the graph structure for the centrality visualization. Must include a \code{graph} element suitable for \code{qgraph}.}
\item{groups}{A vector or list specifying groups of nodes for coloring in the centrality graph.}
\item{error_Model}{A vector containing pie chart proportions or other graphical representations for the nodes in the \code{qgraph} visualization.}
\item{ncol}{Number of columns in the combined visualization layout. Default is \code{2}.}
\item{widths}{Relative widths of the two plots in the layout. Default is \code{c(0.50, 0.60)}.}
\item{output_path}{File path to save the combined visualization as a high-resolution JPEG. Default is \code{"Output/Figura_1_Final.jpg"}.}
\item{show_plot}{Logical. If \code{TRUE}, the combined visualization is displayed in the RStudio plot window. If \code{FALSE}, it is saved to the specified \code{output_path}. Default is \code{FALSE}.}
}

\details{
The function first generates a centrality graph using \code{qgraph}, saving it as a temporary PNG file. The image is then loaded into R as a raster object using the \code{png} package. This raster object is combined with the provided \code{Figura1_Derecha} plot into a single layout using the \code{gridExtra} package.

The combined visualization can either be displayed in the RStudio plot window (\code{show_plot = TRUE}) or saved as a high-resolution JPEG file (\code{show_plot = FALSE}). Temporary files created during the process are automatically deleted.

The centrality graph includes flexible options for node grouping, layout, and edge labeling, making it suitable for network analysis visualizations.
}

\value{
The function does not return any values. The output is either:
\itemize{
\item A plot displayed in the RStudio plot window (\code{show_plot = TRUE}).
\item A saved high-resolution JPEG image at the specified \code{output_path} (\code{show_plot = FALSE}).
}
}

\examples{

Example usage of combine_graphs_centrality
Simulate a network object for testing
network <- list(graph = matrix(sample(0:1, 100, replace = TRUE), 10, 10))
groups <- rep(1:2, each = 5)
error_Model <- runif(10, 0, 1)

Create a dummy ggplot for Figura1_Derecha
library(ggplot2)
Figura1_Derecha <- ggplot(data.frame(x = 1:10, y = rnorm(10)), aes(x, y)) +
geom_point() + theme_minimal()

Combine and save the visualization
combine_graphs_centrality(Figura1_Derecha = Figura1_Derecha,
network = network,
groups = groups,
error_Model = error_Model,
ncol = 2,
widths = c(0.50, 0.60),
output_path = "Output/Figura_1_Final.jpg",
show_plot = FALSE)

Combine and display the visualization in RStudio
combine_graphs_centrality(Figura1_Derecha = Figura1_Derecha,
network = network,
groups = groups,
error_Model = error_Model,
ncol = 2,
widths = c(0.50, 0.60),
show_plot = TRUE)
}

\author{
Dr. José Ventura-León
}
