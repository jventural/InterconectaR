\name{plot_centrality_by_group}
\alias{plot_centrality_by_group}
\title{Plot Centrality Measures for Networks Across Groups}

\description{
The \code{plot_centrality_by_group} function visualizes centrality measures from network models across two groups. It creates a comparative line plot showing the z-scores of the specified centrality measure for each node, allowing for a clear comparison between groups.

\strong{Key Features}:
\itemize{
  \item Processes centrality data from two network models, adjusting group labels for clarity.
  \item Supports custom node labels and flexible color palettes for tailored visualizations.
  \item Outputs a high-quality ggplot object that can be further customized or directly used in presentations.
}
}

\usage{
plot_centrality_by_group(networks_groups,
                         replacements, measure_spec,
                         color_palette = c("#FF5733", "#33FFCE"))
}

\arguments{
\item{networks_groups}{A list containing two network objects (e.g., from \code{bootnet}). These objects are used to compute centrality measures for the nodes.}
\item{replacements}{A character vector of length 2 specifying the names to replace the default group labels (\code{"graph 1"} and \code{"graph 2"}) in the plot.}
\item{measure_spec}{A string indicating the centrality measure to be visualized (e.g., \code{"Betweenness"}, \code{"Closeness"}, or \code{"Strength"}).}
\item{color_palette}{A character vector specifying colors for the groups. Default is \code{c("#FF5733", "#33FFCE")}.}
\item{labels}{Optional. A named vector providing custom labels for the nodes. If not provided, the default node names are used.}
}

\details{
The function computes centrality measures for the nodes in two network models and filters the results to the specified centrality measure (\code{measure_spec}). The group labels in the data are replaced with more descriptive names using the \code{replacements} argument.

Node labels can be customized using the \code{labels} argument, which maps default node names to user-defined labels. The resulting data is visualized using \code{ggplot2} as a line plot, with lines and points representing the z-scores of the centrality measure for each node.

The visualization employs a flip coordinate system (\code{coord_flip()}) for improved readability when dealing with many nodes.
}

\value{
A \code{ggplot2} object representing the centrality comparison plot. The plot includes:
\itemize{
  \item Lines and points for each group, colored according to the specified palette.
  \item Nodes ordered by the mean centrality value for improved interpretability.
  \item Clear labeling of axes and a customizable title based on the specified centrality measure.
}
}

\examples{
# Example usage with simulated data

# Simulating networks
set.seed(123)
network1 <- list(graph = matrix(runif(100, 0, 1), nrow = 10))
network2 <- list(graph = matrix(runif(100, 0, 1), nrow = 10))

# Centrality analysis
networks_groups <- list(network1, network2)
replacements <- c("Group A", "Group B")
measure_spec <- "Strength"

# Create the plot
plot <- plot_centrality_by_group(
  networks_groups = networks_groups,
  replacements = replacements,
  measure_spec = measure_spec,
  color_palette = c("#1f77b4", "#ff7f0e")
)

# Display the plot
print(plot)
}

\author{
Dr. José Ventura-León
}
