\name{combine_graphs_centrality}
\alias{combine_graphs_centrality}
\title{Combine Centrality and Comparative Plots into a Unified Visualization}

\description{
The \code{combine_graphs_centrality} function creates a composite visualization by combining a network centrality graph generated with the \code{qgraph} package and a comparative plot (e.g., a \code{ggplot2} object). The combined visualization can be displayed directly in RStudio or saved as a high-resolution image file.

\strong{Key Features}:
\itemize{
  \item Automatically installs and loads required packages (\code{ggplot2}, \code{qgraph}, \code{gridExtra}, \code{png}, \code{grid}, \code{Cairo}) if they are not already available.
  \item Allows customization of the centrality graph with options for node grouping, layout, edge labels, and more.
  \item Produces flexible layouts for combined visualizations with adjustable column counts and plot widths.
  \item Supports high-resolution output for publications or presentations.
}
}

\usage{
combine_graphs_centrality(Figura1_Derecha, network, groups, error_Model,
                          labels = NULL, ncol = 2, widths = c(0.50, 0.60),
                          output_path = "Output/Figura_1_Final.jpg",
                          show_plot = FALSE)
}

\arguments{
\item{Figura1_Derecha}{A \code{ggplot2} object representing a comparative plot to be included in the final visualization.}
\item{network}{A list containing the network structure for the centrality graph. The list must include a \code{graph} matrix suitable for \code{qgraph}.}
\item{groups}{A vector or list specifying the grouping of nodes for coloring in the centrality graph.}
\item{error_Model}{A vector containing proportions or other data for pie chart visualizations on the nodes in the \code{qgraph} graph.}
\item{labels}{Optional. A vector of custom labels for the nodes in the centrality graph. If \code{NULL}, the default labels in \code{network$labels} are used.}
\item{ncol}{An integer specifying the number of columns in the layout of the combined visualization. Default is \code{2}.}
\item{widths}{A numeric vector specifying the relative widths of the two plots in the layout. Default is \code{c(0.50, 0.60)}.}
\item{output_path}{A string specifying the file path to save the final visualization as a high-resolution JPEG file. Default is \code{"Output/Figura_1_Final.jpg"}.}
\item{show_plot}{Logical. If \code{TRUE}, the visualization is displayed in the RStudio plot window. If \code{FALSE}, it is saved to the file specified in \code{output_path}. Default is \code{FALSE}.}
}

\details{
The function generates the centrality graph using \code{qgraph} and saves it temporarily as a PNG file. This file is then imported into R as a raster image using the \code{png} package. The rasterized graph is combined with the provided \code{Figura1_Derecha} plot using the \code{gridExtra} package.

The combined output can be displayed in the RStudio plot window (\code{show_plot = TRUE}) or saved as a high-resolution JPEG file (\code{show_plot = FALSE}). The function automatically deletes temporary files after use.

The centrality graph includes a variety of customizable options such as node sizes, edge sizes, color palettes, layout, and the inclusion of pie charts based on \code{error_Model}.
}

\value{
The function does not return a value. Instead, it produces:
\itemize{
  \item A plot displayed in the RStudio plot viewer (\code{show_plot = TRUE}).
  \item A saved high-resolution JPEG file at the location specified in \code{output_path} (\code{show_plot = FALSE}).
}
}

\examples{
# Example: Simulating a network for testing
network <- list(graph = matrix(sample(0:1, 100, replace = TRUE), 10, 10), labels = LETTERS[1:10])
groups <- rep(1:2, each = 5)
error_Model <- runif(10, 0, 1)

# Creating a ggplot object for Figura1_Derecha
library(ggplot2)
Figura1_Derecha <- ggplot(data.frame(x = 1:10, y = rnorm(10)), aes(x, y)) +
  geom_point() + theme_minimal()

# Saving the combined visualization
combine_graphs_centrality(Figura1_Derecha = Figura1_Derecha,
                          network = network,
                          groups = groups,
                          error_Model = error_Model,
                          ncol = 2,
                          widths = c(0.50, 0.60),
                          output_path = "Output/Figura_1_Final.jpg",
                          show_plot = FALSE)

# Displaying the combined visualization in RStudio
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
