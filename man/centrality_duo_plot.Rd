\name{centrality_duo_plot}
\alias{centrality_duo_plot}
\title{Plot One or Two Centrality Measures for Network Visualization}

\description{
The \code{centrality_duo_plot} function generates comparative plots of one or two centrality-related measures
for nodes in a psychological or social network estimated with \code{qgraph}. It allows flexible inclusion of
standard centrality indices (e.g., \code{"ExpectedInfluence"}) and bridge measures (e.g., \code{"bridgeExpectedInfluence"})
computed via the \code{networktools} package.

The function is designed to create publication-ready visualizations that show how nodes rank
across one or two measures, such as general centrality and bridge influence, using customizable
colors and labels.

\strong{Key Features}:
\itemize{
\item Plots a single centrality metric or compares two metrics simultaneously (e.g., Expected Influence vs. Bridge Expected Influence).
\item Automatically integrates results from \code{centralityTable()} and \code{bridge()}.
\item Accepts user-defined node labels or abbreviations for clearer visualization.
\item Provides customizable color palettes, label handling, and grouping support for bridge measures.
\item Returns both the data table of computed measures and a ready-to-publish \code{ggplot2} figure.
}
}

\usage{
centrality_duo_plot(
  qgraph_obj,
  network,
  groups = NULL,
  measure0 = "ExpectedInfluence",
  measure1 = NULL,
  color_palette = c("#6C5CE7", "#00B894"),
  use_abbrev = TRUE,
  labels = NULL
)
}

\arguments{
\item{\code{qgraph_obj}}{A \code{qgraph} object representing the network to visualize. Must contain node labels if no custom labels are provided.}
\item{\code{network}}{A network object (typically from \code{bootnet::estimateNetwork}) that includes the adjacency matrix in \code{network\$graph}.}
\item{\code{groups}}{An optional vector indicating community membership for each node. Required if a bridge measure (\code{measure1}) is specified.}
\item{\code{measure0}}{A character string naming the primary centrality measure to plot (default: \code{"ExpectedInfluence"}).}
\item{\code{measure1}}{Optional; a character string naming the secondary (bridge) measure to plot, e.g., \code{"bridgeExpectedInfluence"}.}
\item{\code{color_palette}}{A character vector of one or two colors used for plotting the measures. Default is a purple-green palette.}
\item{\code{use_abbrev}}{Logical; if \code{TRUE}, abbreviated labels (from \code{qgraph} or user input) are used on the y-axis. Default is \code{TRUE}.}
\item{\code{labels}}{Optional character vector with custom node labels to override those in \code{qgraph}. Must match the number of nodes.}
}

\details{
The function first computes the selected centrality measure using \code{centralityTable()}, extracts node-level values,
and aligns them with the graph labels. If a bridge measure is requested (\code{measure1} not \code{NULL}),
the function calls \code{networktools::bridge()} to compute the corresponding standardized values.

The two measures are then merged, scaled to z-scores, and plotted side-by-side using \code{ggplot2}.
If \code{measure1} is omitted, a single-measure line plot is created.

This approach facilitates quick comparison between within-network influence (e.g., Expected Influence)
and between-community connectivity (e.g., Bridge Expected Influence), allowing researchers to identify
nodes that are central overall or serve as connectors across communities.

\strong{Graphical Output:}
\itemize{
\item When both measures are supplied, the plot shows two connected points per node, colored by measure.
\item When a single measure is supplied, a simple ranked line plot is displayed.
}
}

\value{
A list containing:
\itemize{
  \item \code{$table}: A tibble with node names, abbreviations, and computed values for each metric.
  \item \code{$plot}: A \code{ggplot2} object showing the centrality and/or bridge measures.
}
}

\examples{
\dontrun{
# Example: Compare Expected Influence and Bridge Expected Influence
result <- centrality_duo_plot(
  qgraph_obj = net_qgraph,
  network = net_estimate,
  groups = c(1,1,2,2,3,3),
  measure0 = "ExpectedInfluence",
  measure1 = "bridgeExpectedInfluence",
  color_palette = c("#0984e3", "#d63031"),
  use_abbrev = TRUE
)

# Show table and plot
result$table
print(result$plot)
}
}

\author{
Dr. José Ventura-León
}
