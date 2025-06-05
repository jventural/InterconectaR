\name{estimate_networks_by_group}
\alias{estimate_networks_by_group}
\title{Estimate Networks Separately for Each Group in a Dataset}

\description{
The \code{estimate_networks_by_group} function facilitates the estimation of network models separately for each group in a dataset. Using the \code{bootnet} package, it automatically partitions the data by a grouping variable, optionally abbreviates column names for readability, estimates networks for the specified columns within each group, and returns the results as a list of network objects.

\strong{Key Features}:
\itemize{
\item Automatically handles data filtering and grouping, ensuring efficient preparation for network estimation.
\item Supports flexible correlation methods (\code{spearman}, \code{pearson}, etc.).
\item Employs the \code{bootnet::estimateNetwork} function to estimate networks, with default model selection or stepwise procedures.
\item Optionally abbreviates variable names (via \code{abbreviate()}) before estimation to produce shorter, unique node labels.
\item Fully compatible with grouped data structures, enabling seamless integration in comparative network analysis.
}
}

\usage{
estimate_networks_by_group(data, group_var, columns,
default = "ggmModSelect",
stepwise = TRUE,
corMethod = "spearman",
abbreviate_vars = FALSE,
abbr_minlength = 3)
}

\arguments{
\item{\code{data}}{A \code{data.frame} containing the variables to estimate the networks and the grouping variable.}
\item{\code{group_var}}{A string specifying the name of the grouping variable in \code{data}. This variable determines the groups for separate network estimation.}
\item{\code{columns}}{A character vector specifying the names of the columns to include in the network estimation (selected from \code{data}).}
\item{\code{default}}{The default method for network estimation, passed directly to \code{bootnet::estimateNetwork}. Default is \code{"ggmModSelect"}.}
\item{\code{stepwise}}{Logical. If \code{TRUE}, enables the stepwise procedure during network estimation. Default is \code{TRUE}.}
\item{\code{corMethod}}{A string specifying the correlation method to use in the network estimation. Options include \code{"spearman"}, \code{"pearson"}, and others supported by \code{cor}. Default is \code{"spearman"}.}
\item{\code{abbreviate_vars}}{Logical. If \code{TRUE}, applies \code{abbreviate()} to the column names before estimating each network, producing shorter, unique labels. Default is \code{FALSE}.}
\item{\code{abbr_minlength}}{An integer specifying the minimum length of each abbreviation when \code{abbreviate_vars = TRUE}. Default is \code{3}.}
}

\details{
The function first identifies unique groups within the \code{group_var} column of the dataset. It then partitions the data by these groups and prepares each subset by selecting only the specified \code{columns}.

If \code{abbreviate_vars = TRUE}, each subset’s column names are passed through \code{abbreviate(colnames, minlength = abbr_minlength)} to generate unique, shortened variable names. Those abbreviated names are used as node labels in the network estimation. After calling \code{bootnet::estimateNetwork()}, the function also ensures that any returned components (e.g., \code{net$data} or \code{dimnames(net$graph)}) reflect the same abbreviated names.

Finally, \code{bootnet::estimateNetwork()} is run on each (possibly renamed) subset using the specified \code{default}, \code{stepwise}, and \code{corMethod} arguments. The result is a named list of network objects, one per group, where each element corresponds to the estimated network for that group and carries the (possibly abbreviated) variable names.
}

\value{
A named list of network objects. Each element in the list represents the estimated network for a specific group, with names corresponding to the unique values in \code{group_var}. If \code{abbreviate_vars = TRUE}, the network objects’ internal data frames and adjacency matrices use the abbreviated column names.
}

\examples{
\dontrun{

# Estimate networks by group without abbreviation
networks <- estimate_networks_by_group(
data = example_data,
group_var = "Sexo",
columns = c("Abandono", "Autonomia", "Necesidad", "Miedo Soledad", "Bienestar"),
default = "ggmModSelect",
stepwise = TRUE,
corMethod = "spearman",
abbreviate_vars = FALSE
)

# Access the network object for group "Mujer"
networks[["Mujer"]]

# Estimate networks by group with abbreviated variable names (minimum 3 characters)
networks_abbrev <- estimate_networks_by_group(
data = example_data,
group_var = "Sexo",
columns = c("Abandono", "Autonomia", "Necesidad", "Miedo Soledad", "Bienestar"),
default = "ggmModSelect",
stepwise = TRUE,
corMethod = "spearman",
abbreviate_vars = TRUE,
abbr_minlength = 3
)

# Check that in the "Mujer" network, the data columns are abbreviated
colnames(networks_abbrev$Mujer$data)

Possible output: "Aba", "Aut", "Nec", "Mie", "Bie"
}
}

\author{
Dr. José Ventura-León
}
