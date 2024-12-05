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
\item Combines the results into a single table with standardized z-scores for both centrality types.
\item Generates a comparative plot using \code{ggplot2}, with nodes ordered by centrality and connected by lines across the measures.
\item Supports customization of node labels and plot colors for enhanced interpretability.
}

The function automatically installs and loads the following required libraries if they are not already available: \code{qgraph}, \code{dplyr}, \code{networktools}, \code{tibble}, \code{ggplot2}, and \code{tidyr}.

\value{
A list with the following components:
\itemize{
\item \code{table}: A dataframe with combined centrality measures, including:
\itemize{
\item \code{node}: Node identifiers.
\item \code{Item}: Node labels for visualization.
\item \code{measure1}: Scaled bridge centrality values (z-scores).
\item \code{measure0}: Standard centrality values (z-scores).
}
\item \code{plot}: A \code{ggplot2} object visualizing the comparative centrality measures for all nodes, with lines connecting corresponding nodes across the measures.
}
}

\examples{

Example network setup
library(qgraph)
library(igraph)

Create example network objects
set.seed(123)
qgraph_obj <- igraph::make_ring(10) # Example igraph object
network <- qgraph::qgraph(matrix(rnorm(100), 10, 10)) # Example qgraph object

Calculate and visualize centrality measures
result <- centrality_plots(
qgraph_obj = qgraph_obj,
network = network,
measure0 = "ExpectedInfluence",
measure1 = "Bridge Expected Influence (1-step)"
)

View the results
print(result$table) # Centrality table
print(result$plot) # Comparative plot
}

\author{
Dr. José Ventura-León
}
