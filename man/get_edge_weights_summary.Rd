\name{get_edge_weights_summary}
\alias{get_edge_weights_summary}
\title{Calculate Descriptive Statistics for Edge Weights in a Network}

\description{
This function calculates descriptive statistics for the edge weights of a given network. The output includes the mean, standard deviation, minimum, and maximum of the edge weights, returned in a single dataframe for easy analysis. Internally, the function ensures that only unique edges are considered in symmetric matrices, diagonal values are excluded, and zero weights are dropped.
}

\usage{
get_edge_weights_summary(network, abs_weights = FALSE, round_digits = 2)
}

\arguments{
  \item{network}{A network object containing the graph structure. This is used to calculate the edge weight matrix. The network must have a \code{graph} attribute compatible with the \code{getWmat()} function.}
  \item{abs_weights}{Logical. If \code{TRUE}, absolute values of edge weights are used in the calculations. Defaults to \code{FALSE}.}
  \item{round_digits}{Integer or \code{NULL}. Number of decimal places to round the results for presentation. If \code{NULL}, no rounding is applied. Defaults to \code{2}.}
}

\details{
The function extracts the edge weight matrix from the network object using \code{getWmat()} and applies the following internal rules:
\itemize{
  \item Only unique edges are considered when the matrix is symmetric.
  \item The diagonal of the matrix is excluded.
  \item Zero-valued edges are removed.
}

After preprocessing, the edge weight vector is optionally transformed to absolute values (\code{abs_weights = TRUE}) and used to compute the following descriptive statistics:
\itemize{
  \item \code{Mean}: The average weight of the selected edges.
  \item \code{SD}: The standard deviation of the edge weights.
  \item \code{Min}: The smallest edge weight.
  \item \code{Max}: The largest edge weight.
}

The results are returned in a dataframe with one row, providing a concise summary for interpretation and further analysis.
}

\value{
A dataframe containing the following columns:
\itemize{
  \item \code{N}: The number of edges included in the calculations.
  \item \code{Mean}: The mean of the edge weights.
  \item \code{SD}: The standard deviation of the edge weights.
  \item \code{Min}: The minimum edge weight.
  \item \code{Max}: The maximum edge weight.
}
}

\examples{
# Example usage with a network object
# Assuming 'network' is a pre-defined network object with a valid graph structure

# Calculate descriptive statistics for edge weights
result <- get_edge_weights_summary(network)

# View the results
print(result)

# Use absolute weights
result_abs <- get_edge_weights_summary(network, abs_weights = TRUE)
print(result_abs)
}

\author{
Dr. José Ventura-León
}
