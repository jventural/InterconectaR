\name{estimate_networks_by_group}
\alias{estimate_networks_by_group}
\title{Estimate Networks Separately for Each Group in a Dataset}

\description{
The \code{estimate_networks_by_group} function facilitates the estimation of network models separately for each group in a dataset. Using the \code{bootnet} package, it automatically partitions the data by a grouping variable, estimates networks for the specified columns within each group, and returns the results as a list of network objects.

\strong{Key Features}:
\itemize{
  \item Automatically handles data filtering and grouping, ensuring efficient preparation for network estimation.
  \item Supports flexible correlation methods (\code{spearman}, \code{pearson}, etc.).
  \item Employs the \code{bootnet::estimateNetwork} function to estimate networks, with default model selection or stepwise procedures.
  \item Fully compatible with grouped data structures, enabling seamless integration in comparative network analysis.
}
}

\usage{
estimate_networks_by_group(data, group_var, columns,
                           default = "ggmModSelect",
                           stepwise = TRUE,
                           corMethod = "spearman")
}

\arguments{
\item{data}{A \code{data.frame} containing the variables to estimate the networks and the grouping variable.}
\item{group_var}{A string specifying the name of the grouping variable in \code{data}. This variable determines the groups for separate network estimation.}
\item{columns}{A character vector specifying the names of the columns to include in the network estimation.}
\item{default}{The default method for network estimation. Passed directly to \code{bootnet::estimateNetwork}. Default is \code{"ggmModSelect"}.}
\item{stepwise}{Logical. If \code{TRUE}, enables the stepwise procedure during network estimation. Default is \code{TRUE}.}
\item{corMethod}{A string specifying the correlation method to use in the network estimation. Options include \code{"spearman"}, \code{"pearson"}, and others supported by \code{cor}. Default is \code{"spearman"}.}
}

\details{
The function first identifies unique groups within the \code{group_var} column of the dataset. It then partitions the data by these groups and prepares the data for network estimation by selecting only the specified \code{columns} for each group.

Using the \code{bootnet::estimateNetwork} function, the networks are estimated separately for each group. The function allows for customizable estimation settings, including the choice of correlation method and whether to use stepwise selection.

The result is a list of network objects, where each element corresponds to the estimated network for a specific group. This structure is well-suited for downstream analysis, such as comparing network properties across groups.
}

\value{
A named list of network objects. Each element in the list represents the estimated network for a specific group, with the names corresponding to the unique values in \code{group_var}.
}

\examples{
# Example dataset
set.seed(123)
example_data <- data.frame(
  group = rep(c("A", "B"), each = 50),
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100)
)

# Estimate networks by group
networks <- estimate_networks_by_group(data = example_data,
                                        group_var = "group",
                                        columns = c("var1", "var2", "var3"),
                                        default = "ggmModSelect",
                                        stepwise = TRUE,
                                        corMethod = "spearman")

# Access the network for group "A"
networks[["A"]]
}

\author{
Dr. José Ventura-León
}
