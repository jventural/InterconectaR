\name{optimize_leiden_resolution}
\alias{optimize_leiden_resolution}
\title{Optimize Leiden Resolution Parameter for Exploratory Graph Analysis}

\description{
This function optimizes the resolution parameter for the Leiden community detection algorithm in Exploratory Graph Analysis (EGA). It evaluates multiple values of the resolution parameter (gamma) and selects the optimal value based on the Total Entropy Fit Index (TEFI).
}

\usage{
optimize_leiden_resolution(data,
                           corr = "cor_auto",
                           gamma_values = c(0.01, 0.05, 0.1, 0.5, 1.0),
                           objective_function = "CPM",
                           verbose = TRUE)
}

\arguments{
  \item{data}{A dataframe containing the variables to be analyzed. The variables should be numeric and suitable for correlation analysis.}

  \item{corr}{A character string specifying the correlation method to be used in the EGA model. Possible values include \code{"cor_auto"}, \code{"pearson"}, and \code{"spearman"}. Default is \code{"cor_auto"}.}

  \item{gamma_values}{A numeric vector specifying the resolution parameter values to be tested in the Leiden algorithm. Default values range from \code{0.01} to \code{1.0}.}

  \item{objective_function}{A character string specifying the objective function to be used in the Leiden algorithm. Default is \code{"CPM"} (Constant Potts Model).}

  \item{verbose}{A logical value indicating whether to display messages about the optimization process. Default is \code{TRUE}.}
}

\details{
The function iterates through a series of resolution parameter values, applies the Leiden clustering algorithm using the EGA framework, and evaluates each model based on the TEFI score. The optimal resolution parameter is selected based on the lowest TEFI value.

If the TEFI value is \code{NaN} for a particular gamma, the function identifies the highest gamma value that produces a valid result and stops further evaluations.

The function returns detailed results, including the best gamma value, a comparison of all tested values, and a visualization of the optimization process.
}

\value{
A list containing the following components:

\item{best_gamma}{The gamma value that yielded the lowest TEFI, considered the optimal parameter.}

\item{max_valid_gamma}{The highest gamma value that produced a valid TEFI result before encountering \code{NaN} values.}

\item{best_model}{The EGA model corresponding to the optimal gamma value.}

\item{all_results}{A list of EGA objects for each tested gamma value.}

\item{comparison}{A data frame comparing the tested gamma values, including TEFI scores, the number of detected communities, and convergence status.}

\item{optimization_plot}{A ggplot2 visualization of the optimization process showing TEFI values across different gamma values.}
}

\examples{
# Cargar datos bfi de la librería psych
library(psych)
data(bfi)
data_bfi <- na.omit(bfi[, 1:25])  # Selección de variables y eliminación de valores faltantes

# Ejecutar optimización de gamma para Leiden
resultados_opt <- optimize_leiden_resolution(
  data = data_bfi,
  corr = "pearson",
  gamma_values = seq(0.01, 1.0, by = 0.1),  # Prueba 10 valores
  verbose = TRUE
)

# Ver el mejor valor de gamma
print(resultados_opt$best_gamma)

# Imprimir la tabla comparativa de los resultados
print(resultados_opt$comparison)

# Visualizar el proceso de optimización
plot(resultados_opt$optimization_plot)

# Acceder al mejor modelo y número de comunidades detectadas
mejor_modelo <- resultados_opt$best_model
mejor_modelo$n.dim  # Número de comunidades detectadas
}

\author{
Dr. José Ventura-León
}
