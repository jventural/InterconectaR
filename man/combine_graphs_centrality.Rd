\name{combine_graphs_centrality}
\alias{combine_graphs_centrality}
\title{Combine Centrality and Comparative Plots into a Unified ggplot Object}

\description{
The \code{combine_graphs_centrality} function creates a composite \code{ggplot2} object by combining a centrality network graph (rendered via \code{qgraph}) with a comparative \code{ggplot2} plot. Instead of writing directly to disk, it returns a \code{ggplot} object (of a custom class that suppresses the common “No shared levels found…” warning) so that you can further annotate it or save it using \code{ggsave()}.

\strong{Key Features}:
\itemize{
\item Integrates a centrality network (produced by \code{qgraph})—embedded as a raster,—with a separate \code{ggplot2} visualization into one layout via \code{patchwork}.
\item Automatically checks for and installs required packages (\code{ggplot2}, \code{qgraph}, \code{png}, \code{grid}, \code{patchwork}, \code{Cairo}).
\item Suppresses the “No shared levels found between \code{names(values)} of the manual scale and the data’s colour values” warning by assigning a custom \code{"silent_gg"} class and print method.
\item Allows fine control over \code{qgraph} aesthetics (node grouping, labels, edge‐label settings, pie slices from \code{error_Model}, layout parameters, etc.).
\item Returns a single \code{ggplot} object that can be modified or saved externally (e.g., with \code{ggsave()}).
}
}

\usage{
combine_graphs_centrality(Figura1_Derecha, network, groups, error_Model,
labels = NULL,
ncol = 2, widths = c(0.50, 0.60),
dpi = 600, legend.cex = 0.1)
}

\arguments{
\item{\code{Figura1_Derecha}}{A \code{ggplot2} object representing the comparative plot to be integrated into the final visualization.}
\item{\code{network}}{A list containing the network structure for the centrality graph. Must include:
\itemize{
\item \code{graph}: a square adjacency matrix (numeric) suitable for \code{qgraph}.
\item \code{labels}: a character vector of node labels (one per row/column of \code{graph}).
}}
\item{\code{groups}}{A vector (or list) of the same length as the number of nodes, specifying membership for coloring nodes in the \code{qgraph} visualization.}
\item{\code{error_Model}}{A numeric vector of length equal to the number of nodes; values are used for pie‐chart slices on each node in the network graph.}
\item{\code{labels}}{Optional. A character vector of custom node labels. Defaults to \code{network$labels} if \code{NULL}.}
\item{\code{ncol}}{An integer indicating the number of columns in the combined layout. Default: \code{2}.}
\item{\code{widths}}{A numeric vector of length 2 specifying the relative widths of the centrality panel and the comparative panel. Default: \code{c(0.50, 0.60)}.}
\item{\code{dpi}}{An integer specifying the resolution (dots per inch) for the temporary PNG output of the \code{qgraph} graph. Default: \code{600}.}
\item{\code{legend.cex}}{A numeric value controlling the size of the legend text in the centrality graph. Default: \code{0.1}.}
}

\details{
When you call \code{combine_graphs_centrality}, the function will:
\enumerate{
\item Check for (and install, if missing) the packages: \code{ggplot2}, \code{qgraph}, \code{png}, \code{grid}, \code{patchwork}, and \code{Cairo}.
\item Render the centrality network via \code{qgraph} into a temporary PNG at the specified \code{dpi}.
\item Read that PNG back into R as a \code{rasterGrob} (using \code{png::readPNG()} and \code{grid::rasterGrob()}) and place it into a blank \code{ggplot} canvas via \code{annotation_custom()}.
\item Combine this rasterized network plot (\code{p1}) with the provided \code{Figura1_Derecha} using \code{patchwork::plot_layout()}, respecting \code{ncol} and \code{widths}.
\item Assign a custom class \code{"silent_gg"} and install a \code{print.silent_gg} method that wraps \code{suppressWarnings()}, thereby hiding any “No shared levels found…” messages when printing or saving.
\item Automatically delete the temporary PNG file.
}
Since the function returns a composed \code{ggplot} object, you can then save it (for example, with \code{ggsave()}) or add further layers, titles, or annotations as needed.}

\value{
A \code{ggplot} object of class \code{"silent_gg"}.
When printed or saved, warnings about “No shared levels found…” will be suppressed. You can pass this object directly to \code{ggsave()} or continue modifying it with additional \code{ggplot2} code.}

\examples{

# Simulated adjacency matrix and labels
adj_mat <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
diag(adj_mat) <- 0
network <- list(graph = adj_mat, labels = LETTERS[1:10])
groups <- rep(1:2, each = 5)
error_Model <- runif(10, 0, 1)

# Create a comparative ggplot2 object
library(ggplot2)
Figura1_Derecha <- ggplot(data.frame(x = 1:10, y = rnorm(10)), aes(x, y)) +
geom_point(size = 3) +
theme_minimal() +
labs(title = "Comparative Plot")

# Combine into a single ggplot object
combined_plot <- combine_graphs_centrality(
Figura1_Derecha = Figura1_Derecha,
network = network,
groups = groups,
error_Model = error_Model,
labels = NULL,
ncol = 2,
widths = c(0.5, 0.5),
dpi = 300,
legend.cex = 0.2
)

# Save using ggsave (15" × 6.5" at 300 dpi)
ggsave(
filename = "combined_centrality_plot.png",
plot = combined_plot,
width = 15,
height = 6.5,
units = "in",
dpi = 300
)
}

\author{
Dr. José Ventura-León
}
