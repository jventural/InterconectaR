\name{filter_correlation_stability}
\alias{filter_correlation_stability}
\title{Filter Stability of Correlation Metrics for Centrality Measures}

\description{
This function calculates the stability of correlation metrics for centrality measures, filters key metrics, and returns a rounded table of the results.

\strong{Note}: This function automatically installs and loads the required libraries if they are not already available. The required libraries are \code{dplyr} and \code{tibble}.
}

\usage{
filter_correlation_stability(caseDroppingBoot)
}

\arguments{
\item{caseDroppingBoot}{An object containing the results of a case-dropping bootstrap analysis. This object is used to compute correlation stability using \code{corStability()}.}
}

\details{
The function performs the following steps:
\itemize{
\item Calculates the correlation stability using \code{corStability()}.
\item Converts the result into a \code{data.frame}.
\item Rounds the correlation stability values to two decimal places.
\item Filters the results to include only key metrics: \code{"bridgeExpectedInfluence"}, \code{"bridgeStrength"}, \code{"expectedInfluence"}, and \code{"strength"}.
}

This is particularly useful for analyzing the robustness of centrality measures in network analysis.
}

\value{
A \code{data.frame} containing the filtered and rounded stability values for the selected metrics:
\itemize{
\item \code{rowname}: Names of the metrics.
\item \code{Index}: Rounded stability values for each metric.
}
}

\examples{

# Example usage of filter_correlation_stability
# Simulate a caseDroppingBoot object for testing
caseDroppingBoot <- list() # Replace with an actual object

# Run the function
result <- filter_correlation_stability(caseDroppingBoot)

# Print the filtered results
print(result)
}

\author{
Dr. José Ventura-León
}

