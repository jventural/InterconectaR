\name{convert_EGA_list_to_df}
\alias{convert_EGA_list_to_df}
\title{Convert a List of EGA Results to a Data Frame}

\description{
This function converts a list of EGA results into a structured data frame, making it easier to analyze and compare multiple model combinations. The function extracts key network metrics and analysis parameters from each EGA result and organizes them into a tabular format.
}

\usage{
convert_EGA_list_to_df(ega_list)
}

\arguments{
  \item{ega_list}{A list of EGA results obtained from multiple correlation and algorithm combinations. Each element in the list should be an object returned by the \code{EGA} function.}
}

\details{
The function processes a list of EGA results, filtering out any \code{NULL} entries to ensure valid data is included. It then applies the \code{convert_EGA_to_df} function to extract key network metrics from each valid result.

The resulting data frame contains relevant information for each model combination, such as:

\itemize{
  \item Model type used (e.g., Glasso).
  \item Correlation method applied (e.g., Pearson, Spearman, cor\_auto).
  \item Community detection algorithm used (e.g., Leiden, Louvain, Walktrap).
  \item Regularization parameter (\code{lambda}).
  \item Number of nodes and edges in the network.
  \item Network density and edge weight statistics.
  \item Number of detected communities.
  \item The Total Entropy Fit Index (TEFI).
}

Additionally, the function separates the original model combination names (e.g., \code{"pearson.leiden"}) into two separate columns: \code{Correlation} and \code{Algorithm}, facilitating easier comparisons and visualizations.
}

\value{
A data frame with the following columns:

\item{Model_Combination}{The original combination of correlation method and algorithm used (e.g., \code{"pearson.leiden"}).}
\item{Correlation}{The correlation method applied (e.g., Pearson, Spearman).}
\item{Algorithm}{The community detection algorithm used.}
\item{Model}{The network estimation model applied (e.g., Glasso).}
\item{Lambda}{Regularization parameter used for network estimation.}
\item{Nodes}{The number of nodes in the network.}
\item{Edges}{The number of edges (connections) in the network.}
\item{Density}{Proportion of edges present in the network.}
\item{Mean\_Weight}{The average weight of the network edges.}
\item{SD\_Weight}{The standard deviation of edge weights.}
\item{Min\_Weight}{The minimum weight of the network edges.}
\item{Max\_Weight}{The maximum weight of the network edges.}
\item{Communities}{The number of detected communities in the network.}
\item{TEFI}{The Total Entropy Fit Index for the network structure.}
}

\examples{
# Example usage with EGA results list
library(psych)
data(bfi)
data_bfi <- na.omit(bfi[, 1:25])

# Run EGA analysis with multiple methods
result_list <- run_EGA_combinations(
  data = data_bfi,
  corr = c("pearson", "spearman"),
  algorithm = c("leiden", "louvain"),
  leiden_args = list(objective_function = "CPM", resolution_parameter = 0.11)
)

# Convert the list of EGA results to a data frame
df_results <- convert_EGA_list_to_df(result_list)

# Print the resulting data frame
print(df_results)
}

\author{
Dr. José Ventura-León
}
