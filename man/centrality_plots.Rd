\name{centrality_plots}
\alias{centrality_plots}
\title{Calculate and Visualize Centrality and Bridge Centrality Measures for Networks}

\description{
The \code{centrality_plots} function calculates two types of centrality measures—standard node centrality (e.g., \code{Expected Influence}) and bridge centrality (e.g., \code{Bridge Expected Influence (1-step)})—for a given network. It combines these metrics into a unified table and produces a comparative plot for visualizing node-level centrality across the measures.

\strong{Note:} For accurate use, ensure that the \code{measure0} and \code{measure1} arguments match the supported names. Valid names for \code{measure0} include:
\itemize{
\item \code{"Strength"}
\item \code{"ExpectedInfluence"}
}
Valid names for \code{measure1} include:
\itemize{
\item \code{"Bridge Strength"}
\item \code{"Bridge Betweenness"}
\item \code{"Bridge Closeness"}
\item \code{"Bridge Expected Influence (1-step)"}
\item \code{"Bridge Expected Influence (2-step)"}
\item \code{"communities"}
}
Ensure the names match exactly as listed to avoid errors.
}

\usage{
centrality_plots(qgraph_obj, network, measure0 = "ExpectedInfluence", measure1 = "Bridge Expected Influence (1-step)", color_palette = c("#FF0000", "#00A08A"), labels = NULL)
}

\arguments{
\item{qgraph_obj}{An igraph or qgraph object representing the network structure. This object is used to calculate bridge centrality measures.}
\item{network}{A qgraph object used to calculate standard centrality measures, such as \code{Expected Influence}.}
\item{measure0}{A string specifying the centrality measure to calculate for \code{network}. Defaults to \code{"ExpectedInfluence"}. Supported values: \code{"Strength"} and \code{"ExpectedInfluence"}.}
\item{measure1}{A string specifying the bridge centrality measure to calculate for \code{qgraph_obj}. Defaults to \code{"Bridge Expected Influence (1-step)"}. Supported values: \code{"Bridge Strength"}, \code{"Bridge Betweenness"}, \code{"Bridge Closeness"}, \code{"Bridge Expected Influence (1-step)"}, \code{"Bridge Expected Influence (2-step)"}, \code{"communities"}.}
\item{color_palette}{A vector of two colors for the comparative plot, corresponding to \code{measure0} and \code{measure1}. Defaults to \code{c("#FF0000", "#00A08A")}.}
\item{labels}{(Optional) A named vector mapping node identifiers to custom labels for visualization. If \code{NULL}, node names from the network are used.}
}

\details{
This function performs the following steps:
\enumerate{
\item Extracts standard centrality metrics (e.g., \code{Expected Influence}) from the \code{network} using \code{qgraph::centralityTable()} and filters for \code{measure0}.
\item Calculates bridge centrality metrics (e.g., \code{Bridge Expected Influence (1-step)}) for \code{qgraph_obj} using the \code{networktools::bridge()} function.
\description{
\item Combines the results into a single table with standardized z-scores for both centrality types, including an “Abrev” column for three-character node abbreviations.
\item Generates a comparative plot using \code{ggplot2}, with the y-axis labeled “Nodos” and nodes ordered by centrality. Lines connect corresponding nodes across the selected measures.
\item Supports toggling between full node labels or three-character abbreviations via the \code{use_abbrev} argument, as well as customization of node labels, plot colors, and legend labels for enhanced interpretability.
}

The function automatically installs and loads the following required libraries if they are not already available: \code{qgraph}, \code{dplyr}, \code{networktools}, \code{tibble}, \code{ggplot2}, \code{tidyr}, and \code{stringr}.

\value{
A list with the following components:
\itemize{
\item \code{table}: A dataframe containing:
\itemize{
\item \code{full_name}: Original node identifiers (character).
\item \code{Abrev}: Three-character abbreviations of \code{full_name} (character).
\item \code{measure1}: Scaled bridge centrality values (z-scores), present only if \code{measure1} is not \code{NULL}.
\item \code{measure0}: Standard centrality values (z-scores).
}
\item \code{plot}: A \code{ggplot2} object visualizing the comparative centrality measures for all nodes. The y-axis is titled “Nodos,” and either full names or abbreviations are shown depending on \code{use_abbrev}. Lines and points connect each node’s values across \code{measure0} and (if provided) \code{measure1}.
}
}

\examples{
\dontrun{

Example network setup (requires qgraph and igraph)
library(qgraph)
library(igraph)

Create an example igraph ring and convert to qgraph format
set.seed(123)
ig_obj <- igraph::make_ring(10)
adj_mat <- igraph::as_adjacency_matrix(ig_obj, sparse = FALSE)
network <- qgraph::qgraph(adj_mat)

Calculate and visualize centrality measures using full names
result_full <- centrality_plots(
qgraph_obj = ig_obj,
network = network,
groups = rep(1:2, each = 5), # example community vector
measure0 = "ExpectedInfluence",
measure1 = "Bridge Expected Influence (1-step)",
color_palette = c("#FF0000", "#00A08A"),
use_abbrev = FALSE
)

Calculate and visualize centrality measures using abbreviations
result_abbrev <- centrality_plots(
qgraph_obj = ig_obj,
network = network,
groups = rep(1:2, each = 5),
measure0 = "ExpectedInfluence",
measure1 = NULL,
color_palette = c("#1B9E77"),
use_abbrev = TRUE
)

View the results
print(result_full$table) # Centrality table with both measures, full names
print(result_full$plot) # Comparative plot showing full names

print(result_abbrev$table) # Centrality table with abbreviations only
print(result_abbrev$plot) # Plot showing three-character abbreviations
}
}

\author{
Dr. José Ventura-León
}
