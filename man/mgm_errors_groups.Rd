\name{mgm_errors_groups}
\alias{mgm_errors_groups}
\title{Estimate and Compare Prediction Errors for Mixed Graphical Models Across Groups}

\description{
The \code{mgm_errors_groups} function estimates mixed graphical models (MGMs) for subsets of a dataset defined by a grouping variable. For each group, it fits a MGM using the \code{mgm} package and evaluates the prediction errors using metrics such as RMSE, R\textsuperscript{2}, CC (Correct Classification), and nCC (Normalized Correct Classification). The results are returned as a list of error metrics, one for each group.

\strong{Key Features}:
\itemize{
  \item Efficiently handles grouped data, fitting separate MGMs for each group.
  \item Removes columns with missing values automatically to ensure robust model estimation.
  \item Outputs prediction errors in a structured format for easy comparison between groups.
}
}

\usage{
mgm_errors_groups(data, type, level, group, columns)
}

\arguments{
\item{data}{A \code{data.frame} containing the dataset to be analyzed. Must include the grouping variable and the columns for the MGM.}
\item{type}{A character vector specifying the variable types for the MGM (e.g., \code{"g"} for Gaussian, \code{"c"} for categorical, etc.).}
\item{level}{An integer vector specifying the number of levels for each variable in the MGM (e.g., \code{1} for continuous variables or the number of categories for categorical variables).}
\item{group}{A grouping variable in \code{data} that defines the subsets of the dataset for separate MGM estimation.}
\item{columns}{A character vector specifying the column names of the variables to include in the MGM.}
}

\details{
This function performs the following steps:
\enumerate{
  \item Groups the dataset by the specified \code{group} variable.
  \item Fits a MGM for each group using the \code{mgm()} function from the \code{mgm} package.
  \item Removes columns with missing values in each group to avoid errors during model fitting.
  \item Computes prediction errors for each MGM using the \code{predict()} function from the \code{mgm} package.
  \item Returns the prediction error metrics (\code{RMSE}, \code{R2}, \code{CC}, \code{nCC}) as a list, with one element per group.
}

This function is particularly useful for comparing the predictive performance of MGMs across different subpopulations within a dataset.
}

\value{
A named list of \code{tibble}s, where each element contains the error metrics (\code{RMSE}, \code{R2}, \code{CC}, \code{nCC}) for a specific group. The names of the list correspond to the unique values of the grouping variable.
}

\examples{
# Example dataset
set.seed(123)
example_data <- data.frame(
  Sexo = rep(c("Hombre", "Mujer"), each = 50),
  Ansiedad_separación = rnorm(100),
  Expresión_afectiva = rnorm(100),
  Autoestima = rnorm(100)
)

# Estimate MGM errors by group
error_results <- mgm_errors_groups(
  data = example_data,
  type = c("g", "g", "g"),  # Gaussian variables
  level = c(1, 1, 1),       # Continuous variables
  group = Sexo,
  columns = c("Ansiedad_separación", "Expresión_afectiva", "Autoestima")
)

# Access errors for a specific group
error_results[["Hombre"]]
}

\author{
Dr. José Ventura-León
}
