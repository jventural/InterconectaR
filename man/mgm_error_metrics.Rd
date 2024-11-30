\name{mgm_error_metrics}
\alias{mgm_error_metrics}
\title{Calculate Error Metrics for Mixed Graphical Models (MGM)}

\description{
This function calculates error metrics for Mixed Graphical Models (MGM) by fitting a model to a given dataset and extracting metrics related to continuous and categorical variables. The output includes RMSE and R2 for continuous variables, and CC and nCC for categorical variables.
}

\usage{
mgm_error_metrics(data, type, level, k = 2)
}

\arguments{
  \item{data}{A dataframe containing the variables to be used in the MGM model. The variables must be numeric or categorical.}
  \item{type}{A character vector specifying the type of each variable in \code{data}. Use \code{"g"} for Gaussian variables, \code{"c"} for categorical variables, and \code{"p"} for Poisson variables.}
  \item{level}{A numeric vector indicating the number of levels for each variable. For Gaussian variables, use \code{1}. For categorical variables, specify the number of categories.}
  \item{k}{An integer specifying the order of interactions in the MGM model. The default value is \code{2}.}
}

\details{
The function requires the \code{mgm} and \code{dplyr} packages. If these packages are not installed, they will be automatically installed and loaded.

The \code{mgm_error_metrics} function fits an MGM model to the input data and extracts error metrics using the \code{predict()} function from the \code{mgm} package. The metrics include:
\itemize{
  \item \code{RMSE}: Root Mean Squared Error for continuous variables.
  \item \code{R2}: R-squared for continuous variables.
  \item \code{CC}: Classification accuracy for categorical variables.
  \item \code{nCC}: Non-classification accuracy for categorical variables.
}

The function validates that the lengths of \code{type} and \code{level} match the number of columns in \code{data}. The output is a list containing two dataframes: one for continuous error metrics (\code{errorCon}) and one for categorical error metrics (\code{errorCat}).
}

\value{
A list with two components:
\item{errorCon}{A dataframe containing RMSE and R2 for continuous variables. Each row corresponds to a variable.}
\item{errorCat}{A dataframe containing CC and nCC for categorical variables. Each row corresponds to a variable.}
}

\examples{
# Example usage with a sample dataframe

# Define variable types and levels
type <- c(rep("g", 10))
level <- c(rep(1, 10))

# Call the function with a dataframe `df` (should be loaded in advance)
error_Model <- mgm_error_metrics(
  data = df %>% select(`Distorted Love`:`Desire for Sexual Infidelity`),
  type = type,
  level = level
)

# Access error metrics as vectors
error_Model$errorCon$RMSE  # Vector of RMSE
error_Model$errorCon$R2    # Vector of R2
error_Model$errorCat$CC    # Vector of CC
error_Model$errorCat$nCC   # Vector of nCC
}

\author{
Dr. José Ventura-León
}
