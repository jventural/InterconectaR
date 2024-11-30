\name{choose_best_method}
\alias{choose_best_method}
\title{Selection of the Optimal Statistical Method Based on Sample Size and Correlation}

\description{
This function helps users determine the best statistical method for their dataset based on sample size (\code{n}) and the average intercorrelation of a given correlation matrix (\code{r_matrix}). It evaluates the input parameters against pre-defined criteria derived from the literature and provides a recommendation for the most appropriate method.
}

\usage{
choose_best_method(n, r_matrix)
}

\arguments{
  \item{n}{An integer representing the sample size of the dataset.}
  \item{r_matrix}{A square matrix of correlations representing the relationships between variables in the dataset. The matrix must be symmetric and contain numeric values.}
}

\details{
The function first verifies that the input \code{r_matrix} is a square matrix. It then calculates the mean of the upper triangular elements of the matrix (\code{r_mean}), representing the average intercorrelation between variables.

Based on the sample size (\code{n}) and \code{r_mean}, the function recommends the best statistical method. For small sample sizes (\code{n < 100}), the selection is based on low or high correlations. For moderate sample sizes (\code{300 <= n <= 500}), the method is selected based on similar criteria. For large sample sizes (\code{n > 500}), the function accounts for moderate or high correlations.

The method names returned correspond to published approaches, such as Williams (1959), Hendrickson (1970), Steiger (1980), Hittner (2003), Hotelling (1940), and Meng (1992).
}

\value{
A list containing:
\item{best_method}{A character string specifying the recommended statistical method.}
\item{details}{A list with additional information, including \code{n}, \code{r_mean}, and a recommendation message summarizing the selected method.}
}

\examples{
# Example usage with a sample dataset

# Define a correlation matrix
r_matrix <- matrix(c(
  1.00, 0.45, 0.30,
  0.45, 1.00, 0.50,
  0.30, 0.50, 1.00
), nrow = 3, byrow = TRUE)

# Define the sample size
n <- 386

# Call the function
result <- choose_best_method(n, r_matrix)

# View results
print(result$best_method)  # Recommended method
print(result$details)      # Additional details
}

\author{
Dr. José Ventura-León
}
