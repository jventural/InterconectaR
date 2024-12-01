\name{calculate_centrality_sd_correlation}
\alias{calculate_centrality_sd_correlation}
\title{Calculate Centrality Measures, Standard Deviation, and Correlation Analysis}

\description{
This function calculates the standard deviation (SD) of variables from a given dataset and combines it with centrality measures from a provided centrality table. It then performs a correlation analysis between selected centrality measures and the calculated standard deviations.

\strong{Note}: This function automatically installs and loads the required libraries if they are not already available. The required libraries are \code{dplyr}, \code{tibble}, and \code{correlation}.
}

\usage{
calculate_centrality_sd_correlation(Data, Centralitys)
}

\arguments{
\item{Data}{A data frame containing the variables for which the standard deviation (SD) will be calculated. Each column represents a variable.}
\item{Centralitys}{An object containing a \code{table} with centrality measures. The table must include a column named \code{node} to match variable names in \code{Data}.}
}

\details{
The function first calculates the standard deviation (SD) for each column in \code{Data} and converts the results into a data frame with two columns: \code{node} (variable names) and \code{sd} (calculated SD values). It then merges this data frame with the centrality measures provided in \code{Centralitys$table}, using the \code{node} column as the key.

The function dynamically selects the 3rd, 4th, and 5th columns from the merged data frame (\code{combined_data}) for the correlation analysis. Specifically:
\itemize{
\item Columns 3 and 4 are used as predictors in the correlation analysis.
\item Column 5 (SD) is used as the response variable.
}

The correlation analysis is performed using the \code{correlation()} function from the \code{correlation} package.
}

\value{
The function returns the result of the correlation analysis as an object produced by the \code{correlation()} function. This includes correlation coefficients, statistical significance values, and other metrics for the selected columns.
}

\examples{

Example usage of calculate_centrality_sd_correlation
Simulate data for testing
Data <- data.frame(
Variable1 = rnorm(100),
Variable2 = rnorm(100),
Variable3 = rnorm(100)
)

Example centrality table
Centralitys <- list(
table = data.frame(
node = c("Variable1", "Variable2", "Variable3"),
Bridge Expected Influence (1-step) = runif(3),
ExpectedInfluence = runif(3)
)
)

Perform the analysis
result <- calculate_centrality_sd_correlation(Data, Centralitys)

View the result
print(result)
}

\author{
Dr. José Ventura-León
}

