\name{get_edge_weights_summary}
\alias{get_edge_weights_summary}
\title{Calculate Descriptive Statistics for Edge Weights in a Network}

\description{
This function calculates descriptive statistics for the edge weights of a given network. The output includes the mean, standard deviation, minimum, and maximum of the edge weights, returned in a single dataframe for easy analysis.
}

\usage{
get_edge_weights_summary(network)
}

\arguments{
  \item{network}{A network object containing the graph structure. This is used to calculate the edge weight matrix. The network must have a \code{graph} attribute compatible with the \code{getWmat()} function.}
}

\details{
The function extracts the edge weight matrix from the network object using \code{getWmat()}, rounds the values to two decimal places, and calculates the following descriptive statistics:
\itemize{
  \item \code{Mean}: The average weight of all edges.
  \item \code{SD}: The standard deviation of the edge weights.
  \item \code{Min}: The smallest edge weight in the network.
  \item \code{Max}: The largest edge weight in the network.
}

The edge weight matrix is internally flattened into a vector to compute these statistics. The results are returned in a dataframe for convenient interpretation and further analysis.
}

\value{
A dataframe containing the following columns:
\itemize{
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
}

\author{
Dr. José Ventura-León
}
