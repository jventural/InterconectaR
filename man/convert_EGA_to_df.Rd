\name{convert_EGA_to_df}
\alias{convert_EGA_to_df}
\title{Convert EGA Results to Data Frame}

\description{
This function converts the results of an Exploratory Graph Analysis (EGA) into a structured data frame containing key network metrics and analysis parameters. It extracts relevant attributes such as the correlation method, clustering algorithm, and network properties for easy comparison and reporting.
}

\usage{
convert_EGA_to_df(ega_result)
}

\arguments{
  \item{ega_result}{An object returned by the \code{EGA} function, containing the estimated network structure and associated parameters.}
}

\details{
The function processes the \code{ega_result} object by extracting important information from the network matrix and its associated attributes. It computes key network metrics such as:

\itemize{
  \item Model type used (e.g., Glasso).
  \item Correlation method applied (e.g., Pearson, Spearman, cor\_auto).
  \item Community detection algorithm used (e.g., Leiden, Louvain, Walktrap).
  \item Regularization parameter (\code{lambda}) if available.
  \item Number of nodes and edges in the network.
  \item Network density, calculated as the proportion of nonzero edges.
  \item Descriptive statistics of edge weights (mean, standard deviation, minimum, and maximum values).
  \item Number of detected communities.
  \item The Total Entropy Fit Index (TEFI) value, if available.
}

All extracted values are formatted to three decimal places for consistency. If a particular attribute is not present in the input, it is replaced with \code{NA}.
}

\value{
A data frame containing the following columns:

\item{Model}{The model used to estimate the network (e.g., Glasso).}
\item{Correlation}{The correlation method applied (e.g., Pearson, Spearman).}
\item{Algorithm}{The community detection algorithm used.}
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
# Example usage with EGA results
library(psych)
data(bfi)
data_bfi <- na.omit(bfi[, 1:25])

# Run EGA analysis
ega_result <- EGAnet::EGA(
  data = data_bfi,
  corr = "pearson",
  model = "glasso",
  algorithm = "leiden",
  plot.EGA = FALSE
)

# Convert results to a data frame
df_metrics <- convert_EGA_to_df(ega_result)

# Print the results
print(df_metrics)
}

\author{
Dr. José Ventura-León
}
