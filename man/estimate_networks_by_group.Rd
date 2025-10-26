\name{estimate_networks_by_group}
\alias{estimate_networks_by_group}
\title{Estimate Networks Separately for Each Group in a Dataset}

\description{
The \code{estimate_networks_by_group} function estimates network models separately for each group in a dataset.
Using the \code{bootnet} package, it automatically partitions the data by a grouping variable, estimates networks
for specified variables within each group, and allows for optional abbreviation or custom labeling of variable names.
This makes it suitable for comparative network analysis across groups.

\strong{Key Features}:
\itemize{
\item Automatically partitions the dataset by a grouping variable and estimates a network for each subset.
\item Supports flexible correlation methods (\code{"spearman"}, \code{"pearson"}, etc.).
\item Integrates the \code{bootnet::estimateNetwork} function with model selection or stepwise procedures.
\item Allows for automatic variable name abbreviation or user-defined custom labels.
\item Optionally embeds custom labels into network objects’ data and adjacency matrices for consistent visualization.
}
}

\usage{
estimate_networks_by_group(
  data,
  group_var,
  columns,
  default = "ggmModSelect",
  stepwise = TRUE,
  corMethod = "spearman",
  abbreviate_vars = FALSE,
  abbr_minlength = 3,
  labels = NULL,
  set_labels_on_graph = TRUE
)
}

\arguments{
\item{\code{data}}{A \code{data.frame} containing the variables used in network estimation and the grouping variable.}
\item{\code{group_var}}{A string giving the name of the grouping variable that defines the subsets of the data.}
\item{\code{columns}}{A character vector specifying which columns from \code{data} to include in the network estimation.}
\item{\code{default}}{The default network estimation method used in \code{bootnet::estimateNetwork}.
Defaults to \code{"ggmModSelect"}.}
\item{\code{stepwise}}{Logical; if \code{TRUE}, enables the stepwise procedure for network estimation. Default is \code{TRUE}.}
\item{\code{corMethod}}{Specifies the correlation method used for the network estimation (\code{"spearman"}, \code{"pearson"}, etc.).
Default is \code{"spearman"}.}
\item{\code{abbreviate_vars}}{Logical; if \code{TRUE}, abbreviates variable names using \code{abbreviate()}
to create concise node labels. Default is \code{FALSE}.}
\item{\code{abbr_minlength}}{An integer setting the minimum length of abbreviations when
\code{abbreviate_vars = TRUE}. Default is \code{3}.}
\item{\code{labels}}{Optional character vector providing custom labels for variables.
If unnamed, it must match the order of \code{columns}. If named, names must correspond to the original variable names.}
\item{\code{set_labels_on_graph}}{Logical; if \code{TRUE}, applies the final labels to both the adjacency matrix (\code{net\$graph})
and the data matrix (\code{net\$data}) in each network object. Default is \code{TRUE}.}
}

\details{
The function identifies unique groups in \code{group_var}, partitions the dataset accordingly,
and estimates a network for each group using the \code{bootnet::estimateNetwork} function.

It supports three labeling strategies:
\enumerate{
\item Using original variable names (default).
\item Abbreviating names via \code{abbreviate()} when \code{abbreviate_vars = TRUE}.
\item Providing a custom \code{labels} vector, either named (mapping old to new) or ordered to match \code{columns}.
}

If \code{set_labels_on_graph = TRUE}, the function replaces both the \code{dimnames} of \code{net\$graph}
and the \code{colnames} of \code{net\$data} with the final labels, ensuring consistency for later plotting functions.
Additionally, each network object includes an attribute \code{var_mapping} — a \code{tibble} showing
the correspondence between original variable names and applied labels.
}

\value{
A named list of \code{bootnet} network objects. Each element represents one group, labeled by the unique
values of \code{group_var}. Each object contains:
\itemize{
  \item \code{\$graph}: adjacency matrix with labeled row/column names.
  \item \code{\$data}: dataset with updated column names (if applicable).
  \item \code{attr(, "var_mapping")}: a \code{tibble} linking original variable names to final labels.
}
}

\examples{
\dontrun{
# Example with custom labels
networks_groups <- estimate_networks_by_group(
  data = example_data,
  group_var = "Sexo",
  columns = c("Agotamiento_emocional", "Hartazgo", "Control", "Satisfaccion_conyugal"),
  default = "ggmModSelect",
  stepwise = TRUE,
  corMethod = "spearman",
  abbreviate_vars = FALSE,
  labels = c("AgotEmo", "Hartaz", "Contr", "SatCony"),
  set_labels_on_graph = TRUE
)

# Access network for one group
networks_groups$Femenino$graph
attributes(networks_groups$Femenino)$var_mapping
}
}

\author{
Dr. José Ventura-León
}
