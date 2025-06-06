\name{plot_centrality_by_group}
\alias{plot_centrality_by_group}
\title{Plot Centrality Measures for Networks Across Two Groups}

\description{
The \code{plot_centrality_by_group} function visualizes a specified centrality measure from two network models side by side. It generates a comparative line plot of z-scores for each node, allowing for straightforward comparison between the two groups.

\strong{Key Features}:
\itemize{
\item Computes and filters centrality data from two network graphs for a given measure (e.g., “Strength” or “Betweenness”).
\item Replaces default group labels (“graph 1”, “graph 2”) with user-supplied names for clarity.
\item Returns a \code{ggplot2} object that can be further customized or saved with \code{ggsave()}.
}
}

\usage{
plot_centrality_by_group(networks_groups, replacements, measure_spec,
color_palette = c("#FF5733", "#33FFCE"))
}

\arguments{
\item{\code{networks_groups}}{A list of length 2 containing two network objects (for example, as returned by \code{bootnet}). Each list element must include a component \code{$graph} representing the network adjacency matrix.}
\item{\code{replacements}}{A character vector of length 2 specifying the labels to use in place of the default group names “graph 1” and “graph 2”.}
\item{\code{measure_spec}}{A string indicating which centrality measure to plot (for example, \code{"Strength"}, \code{"Betweenness"}, or \code{"Closeness"}).}
\item{\code{color_palette}}{A character vector of length 2 providing the colors to use for each group’s line and points. Default is \code{c("#FF5733", "#33FFCE")}.}
}

\details{
When called, \code{plot_centrality_by_group} performs the following steps:
\enumerate{
\item Calls \code{centralityTable} on each network’s \code{$graph} to extract node centrality values across both groups.
\item Renames the implicit “graph 1” and “graph 2” labels using the two entries in \code{replacements}.
\item Filters the combined centrality table to include only rows where \code{measure} equals \code{measure_spec}.
\item Creates a \code{ggplot2} line plot with \code{geom_line()} and \code{geom_point()}, where:
\itemize{
\item The x-axis lists node names (as a factor reordered by mean centrality value).
\item The y-axis shows each node’s z-score for the specified measure.
\item Lines and points are grouped and colored by the renamed group labels.
}
\item Flips coordinates with \code{coord_flip()} so node names appear on the vertical axis for readability.
}
The resulting plot arranges the two groups’ centrality lines and points side by side for each node, facilitating visual comparison.
}

\value{
A \code{ggplot2} object displaying the centrality comparison. This object includes:
\itemize{
\item A line and point for each node’s z-score in both groups.
\item Nodes ordered by average centrality value (descending) to improve readability.
\item Customizable colors and group labels.
}
Since it returns a \code{ggplot2} object, you can save it with \code{ggsave()} or add additional layers as needed.
}

\examples{
\dontrun{

# Simular dos redes simples con matrices de adyacencia
network1 <- list(graph = matrix(runif(100, 0, 1), nrow = 10))
network2 <- list(graph = matrix(runif(100, 0, 1), nrow = 10))

# Combinar en una lista nombrada
networks_groups <- list(network1, network2)
replacements <- c("Mujer", "Varon")
measure_spec <- "Strength"

# Crear el gráfico comparativo
plot <- plot_centrality_by_group(
networks_groups = networks_groups,
replacements = replacements,
measure_spec = measure_spec,
color_palette = c("#1f77b4", "#ff7f0e")
)

# Mostrar el gráfico
print(plot)

# Guardar con ggsave
ggsave("centrality_comparison.png", plot, width = 8, height = 6, dpi = 300)
}
}

\author{
Dr. José Ventura-León
}
