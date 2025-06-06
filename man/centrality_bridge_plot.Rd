\name{centrality_bridge_plot}
\alias{centrality_bridge_plot}
\title{Visualize and Compare Bridge Centrality Measures Across Network Groups}

\description{
The \code{centrality_bridge_plot} function calculates and visualizes bridge centrality measures (e.g., "Bridge Expected Influence") for networks across two groups. It extracts bridge centrality metrics for both groups, standardizes them as z-scores, and combines the data into a comparative plot. The function also returns the processed centrality data in a tabular format for further analysis.

\strong{Key Features}:
\itemize{
\item Calculates bridge centrality metrics for each group using the \code{networktools} package.
\item Combines bridge centrality data into a unified table with standardized z-scores.
\item Outputs a ggplot object showing the comparative centrality measures, ideal for presentations or publications.
\item Customizable node labels and color palettes for enhanced visualization.
}
}

\usage{
centrality_bridge_plot(
networks_groups,
group_names = c("Mujer", "Varón"),
measure = "Bridge Expected Influence (1-step)",
color_palette = c("#FF5733", "#33FFCE")
)
}

\arguments{
\item{networks_groups}{A named list containing network objects for the groups being compared. The names should correspond to the values in \code{group_names}. Each object should include a \code{graph} matrix and optional \code{labels}.}
\item{group_names}{A character vector of length 2 specifying the names of the groups. Default is \code{c("Mujer", "Varón")}.}
\item{measure}{The bridge centrality measure to compare for the networks (e.g., \code{"Bridge Expected Influence (1-step)"}). Default is \code{"Bridge Expected Influence (1-step)"}.}
\item{color_palette}{A character vector specifying colors for the groups in the plot. Default is \code{c("#FF5733", "#33FFCE")}.}
}

\details{
The function performs the following steps:
\enumerate{
\item Extracts the network graphs for each group from the \code{networks_groups} list.
\item Calculates bridge centrality metrics (e.g., \code{"Bridge Expected Influence"}) for both groups using the \code{networktools::bridge()} function.
\item Combines the metrics into a single table, standardizes the values as z-scores, and formats the data for visualization.
\item Creates a comparative plot using \code{ggplot2}, where nodes are ordered by z-scores and connected across groups.
}

The function does not generate or return network visualizations (\code{qgraph_obj1} or \code{qgraph_obj2}), focusing instead on the tabular and graphical outputs for bridge centrality analysis.
}

\value{
A list with the following elements:
\itemize{
\item \code{table}: A \code{tibble} containing the bridge centrality data for both groups, including standardized z-scores.
\item \code{plot}: A ggplot object showing the comparative bridge centrality measures across the two groups.
}
}

\examples{

Example data setup
\code{set.seed(123)}
\code{network1 <- list(graph = matrix(runif(100, 0, 1), nrow = 10), labels = LETTERS[1:10])}
\code{network2 <- list(graph = matrix(runif(100, 0, 1), nrow = 10), labels = LETTERS[1:10])}
\code{networks_groups <- list("Mujer" = network1, "Varón" = network2)}

Run the function
\code{result <- centrality_bridge_plot(
networks_groups = networks_groups,
group_names = c("Mujer", "Varón"),
measure = "Bridge Expected Influence (1-step)",
color_palette = c("#1f77b4", "#ff7f0e"),
labels = LETTERS[1:10]
)}

Access results
\code{result$plot} # Display the comparative plot
\code{result$table} # View the centrality data
}

\author{
Dr. José Ventura-León
}
