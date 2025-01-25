\name{run_EGA_combinations}
\alias{run_EGA_combinations}
\title{Run Exploratory Graph Analysis with Multiple Combinations}

\description{
This function performs Exploratory Graph Analysis (EGA) using different correlation methods and community detection algorithms. It allows the user to specify various parameter combinations to find the most suitable network structure.
}

\usage{
run_EGA_combinations(data,
                     corr = c("cor_auto", "pearson", "spearman"),
                     algorithm = c("leiden", "louvain", "walktrap"),
                     leiden_args = list(objective_function = "CPM",
                                        resolution_parameter = 1.0))
}

\arguments{
  \item{data}{A dataframe containing the input data for the EGA analysis. The variables should be numeric and continuous.}

  \item{corr}{A character vector specifying the correlation methods to be used. Possible values include \code{"cor_auto"}, \code{"pearson"}, and \code{"spearman"}. Multiple values can be provided.}

  \item{algorithm}{A character vector indicating the community detection algorithms to be used in the analysis. Supported values are \code{"leiden"}, \code{"louvain"}, and \code{"walktrap"}. Multiple values can be provided.}

  \item{leiden_args}{A list of additional parameters specifically for the Leiden algorithm. The default settings include \code{objective_function = "CPM"} and \code{resolution_parameter = 1.0}.}
}

\details{
The function iterates through all combinations of the specified correlation methods and clustering algorithms. For each combination, it applies the EGA procedure using the provided input data.

When the Leiden algorithm is selected, additional parameters from the \code{leiden_args} argument are incorporated to fine-tune the community detection process.

Any errors encountered during the execution are captured and reported, allowing the function to continue evaluating the remaining combinations.

The function returns a list of successful EGA results, along with metadata attributes that store the tested combinations and Leiden-specific arguments.
}

\value{
A list of successful EGA results for each tested combination. Each list element contains an EGA object with the network structure identified under the specified settings.

The result also includes the following attributes:

\item{combinations}{A data frame containing the tested combinations of correlation methods and clustering algorithms.}

\item{leiden_args}{The specific Leiden algorithm parameters used during the analysis.}
}

\examples{
# Ejemplo de uso con datos bfi
library(psych)
data(bfi)
data_bfi <- bfi[, 1:25]  # Seleccionar solo las variables de interés
data_bfi <- na.omit(data_bfi)

# Con parámetros personalizados para Leiden
resultados <- run_EGA_combinations(
  data = data_bfi,
  corr = c("cor_auto", "pearson", "spearman"),
  algorithm = c("leiden", "louvain", "walktrap"),
  leiden_args = list(
    objective_function = "CPM",
    resolution_parameter = 0.11
  )
)
resultados
# Ver combinaciones ejecutadas
attr(resultados, "combinations")
}

\author{
Dr. José Ventura-León
}
