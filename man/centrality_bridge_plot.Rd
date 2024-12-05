\name{centrality_bridge_plot}
\alias{centrality_bridge_plot}
\title{Visualize Bridge Centrality Measures Across Network Groups}

\description{
The \code{centrality_bridge_plot} function creates visualizations and summaries of bridge centrality measures (e.g., "Bridge Expected Influence") for networks across two groups. It generates network visualizations using the \code{qgraph} package, calculates bridge centrality metrics for both groups, and combines these into a comparative line plot with z-scores. The function also returns the underlying centrality data in a tabular format.

\strong{Key Features}:
\itemize{
  \item Generates and customizes network visualizations for each group, including node grouping, layout, and labels.
  \item Computes bridge centrality measures for both groups, facilitating the comparison of node-level influence across communities.
  \item Outputs a ggplot object for bridge centrality measures, ideal for presentations or publications.
  \item Provides the raw centrality data in a standardized format for further analysis.
}
}

\usage{
centrality_bridge_plot(
    networks_groups,
    group_names = c("Mujer", "Varón"),
    groups,
    measure0 = "ExpectedInfluence",
    measure1 = "Bridge Expected Influence (1-step)",
    color_palette = c("#FF5733", "#33FFCE"),
    layout_type = "spring",
    layout_scale = c(0.8, 0.8),
    vsize = 12,
    esize = 12,
    legend_cex = 0.5,
    edge_labels = TRUE,
    labels = NULL
)
}

\arguments{
\item{networks_groups}{A named list containing network objects for the groups being compared. The names should correspond to the values in \code{group_names}.}
\item{group_names}{A character vector of length 2 specifying the group names to label the networks and plots. Default is \code{c("Mujer", "Varón")}.}
\item{groups}{A list or vector specifying the community assignments for the nodes in the networks.}
\item{measure0}{The primary centrality measure to consider (e.g., \code{"ExpectedInfluence"}). Default is \code{"ExpectedInfluence"}.}
\item{measure1}{The bridge centrality measure to compare (e.g., \code{"Bridge Expected Influence (1-step)"}). Default is \code{"Bridge Expected Influence (1-step)"}.}
\item{color_palette}{A character vector specifying colors for the groups in the plot. Default is \code{c("#FF5733", "#33FFCE")}.}
\item{layout_type}{A string specifying the layout type for the network visualization. Default is \code{"spring"}.}
\item{layout_scale}{A numeric vector for scaling the layout of the networks. Default is \code{c(0.8, 0.8)}.}
\item{vsize}{Node size in the network visualization. Default is \code{12}.}
\item{esize}{Edge size in the network visualization. Default is \code{12}.}
\item{legend_cex}{Scaling factor for the legend in the network visualization. Default is \code{0.5}.}
\item{edge_labels}{Logical. If \code{TRUE}, displays edge labels in the network visualization. Default is \code{TRUE}.}
\item{labels}{Optional. A vector of custom labels for the nodes. If \code{NULL}, the default labels in the network objects are used.}
}

\details{
The function performs the following steps:
\enumerate{
  \item Extracts and visualizes the networks for each group using the \code{qgraph} package, applying the specified layout and community grouping.
  \item Computes bridge centrality metrics (e.g., "Bridge Expected Influence") for both networks using the \code{networktools} package.
  \item Combines the centrality measures into a unified table, standardizes the values (z-scores), and formats the data for visualization.
  \item Creates a ggplot object for the bridge centrality measures, with lines and points representing node-level influence across groups.
}

The function returns a list containing the network visualizations, the centrality table, and the comparative plot, making it a comprehensive tool for bridge centrality analysis.
}

\value{
A list with the following elements:
\itemize{
  \item \code{qgraph_obj1}: The network visualization object for the first group.
  \item \code{qgraph_obj2}: The network visualization object for the second group.
  \item \code{table}: A \code{tibble} containing the bridge centrality data for both groups, including z-scores.
  \item \code{plot}: A ggplot object showing the bridge centrality measures for all nodes across the two groups.
}
}

\examples{
# Example data setup
set.seed(123)
network1 <- list(graph = matrix(runif(100, 0, 1), nrow = 10), labels = LETTERS[1:10])
network2 <- list(graph = matrix(runif(100, 0, 1), nrow = 10), labels = LETTERS[1:10])
networks_groups <- list("Mujer" = network1, "Varón" = network2)
groups <- rep(1:2, each = 5)

# Run the function
result <- centrality_bridge_plot(
    networks_groups = networks_groups,
    group_names = c("Mujer", "Varón"),
    groups = groups,
    measure1 = "Bridge Expected Influence (1-step)",
    color_palette = c("#1f77b4", "#ff7f0e"),
    labels = LETTERS[1:10]
)

# Access results
result$plot  # Display the comparative plot
result$table  # View the centrality data
result$qgraph_obj1  # Access the network visualization for the first group
}

\author{
Dr. José Ventura-León
}
