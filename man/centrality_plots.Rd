\name{centrality_plots}
\alias{centrality_plots}
\title{Calculate and Visualize Centrality Measures for Networks}

\description{
This function calculates and visualizes centrality measures for a given network. It combines measures such as \code{Expected Influence} and \code{Bridge Expected Influence (1-step)} into a unified table and produces a comparative plot for visual analysis of node centrality.

\strong{Note}: The valid names for \code{measure0} include \code{"Strength"} and \code{"ExpectedInfluence"}.
For \code{measure1}, valid names include:
\itemize{
\item \code{"Bridge Strength"}
\item \code{"Bridge Betweenness"}
\item \code{"Bridge Closeness"}
\item \code{"Bridge Expected Influence (1-step)"}
\item \code{"Bridge Expected Influence (2-step)"}
\item \code{"communities"}
}
Ensure that these names are provided exactly as listed to avoid errors.
}

\usage{
centrality_plots(qgraph_obj, network, measure0 = "ExpectedInfluence", measure1 = "Bridge Expected Influence (1-step)")
}

\arguments{
\item{qgraph_obj}{An igraph object representing the network structure. This object is used to calculate bridge centrality measures.}
\item{network}{A qgraph object representing the network used to calculate standard centrality measures.}
\item{measure0}{A character string specifying the centrality measure to calculate for \code{network}. The default is \code{"ExpectedInfluence"}. Valid options: \code{"Strength"}, \code{"ExpectedInfluence"}.}
\item{measure1}{A character string specifying the bridge centrality measure to calculate for \code{qgraph_obj}. The default is \code{"Bridge Expected Influence (1-step)"}. Valid options: \code{"Bridge Strength"}, \code{"Bridge Betweenness"}, \code{"Bridge Closeness"}, \code{"Bridge Expected Influence (1-step)"}, \code{"Bridge Expected Influence (2-step)"}, \code{"communities"}.}
}

\details{
The function first calculates standard centrality measures using \code{qgraph::centralityTable()} and filters for the specified measure (\code{measure0}). It then calculates bridge centrality measures using the \code{bridge()} function from the \code{networktools} package, focusing on the specified measure (\code{measure1}).

The results are combined into a single dataframe, which includes scaled values for both centrality types. The function also creates a comparative plot using \code{ggplot2}, where nodes are ordered based on their centrality values, and lines connect corresponding nodes across the two measures.

The function automatically installs and loads the required libraries if they are not already available: \code{qgraph}, \code{dplyr}, \code{networktools}, \code{tibble}, \code{ggplot2}, and \code{tidyr}.
}

\value{
A list with two components:
\item{table}{A dataframe containing the combined centrality measures for all nodes. Columns include:
\itemize{
\item \code{node}: Node names or identifiers.
\item \code{Item}: Node labels for visualization.
\item \code{value_BEI}: Scaled bridge centrality values corresponding to \code{measure1}.
\item \code{value_EI}: Standard centrality values corresponding to \code{measure0}.
}
}
\item{plot}{A ggplot2 object representing a comparative plot of centrality measures. Nodes are ordered based on their centrality values, and lines connect corresponding nodes across the two measures.}
}

\examples{

Example usage with a network
Define igraph and qgraph objects
qgraph_obj <- igraph::make_ring(10) # Example igraph object
network <- qgraph::qgraph(matrix(rnorm(100), 10, 10)) # Example qgraph object

Calculate and visualize centrality measures
result <- centrality_plots(
qgraph_obj = qgraph_obj,
network = network,
measure0 = "ExpectedInfluence",
measure1 = "Bridge Expected Influence (1-step)"
)

View the combined centrality table
print(result$table)

Display the comparative centrality plot
print(result$plot)
}

\author{
Dr. José Ventura-León
}
