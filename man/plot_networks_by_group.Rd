\name{plot_centrality_by_group}
\alias{plot_centrality_by_group}
\title{Plot and Export Centrality Measures by Group Comparison}

\description{
The \code{plot_centrality_by_group} function computes and visualizes node-level centrality metrics
(e.g., strength, expected influence, betweenness) for two network structures, enabling direct
graphical comparison between groups.

Specifically, the function (1) extracts and reformats results from \code{centralityTable()} across
two networks, (2) relabels the group identifiers using user-supplied names, (3) filters the desired
centrality measure (e.g., "Strength" or "Bridge Expected Influence"), and (4) produces a
\pkg{ggplot2} line-and-point plot that displays each node’s standardized value (z-score) for both
groups side by side.

In addition to the plot, it returns the processed data table, facilitating export or further analysis
(e.g., significance testing, ranking, or reporting).
}

\usage{
plot_centrality_by_group(
  networks_groups,
  replacements = c("Group 1", "Group 2"),
  measure_spec = "Strength",
  color_palette = c("#FF5733", "#33FFCE")
)
}

\arguments{
  \item{\code{networks_groups}}{A list of exactly two network objects, each containing a
  \code{$graph} element (as produced by \pkg{EGAnet} or \pkg{bootnet}). The function expects the
  objects to be in the order corresponding to the labels in \code{replacements}.}

  \item{\code{replacements}}{A character vector of length two specifying custom group names to replace
  the default identifiers "graph 1" and "graph 2". These names appear in the legend and plot title.}

  \item{\code{measure_spec}}{A character string indicating which centrality measure to extract and
  plot. Examples include \code{"Strength"}, \code{"ExpectedInfluence"}, or
  \code{"Bridge Expected Influence"}. The name must match exactly one of those returned by
  \code{centralityTable()}.}

  \item{\code{color_palette}}{A character vector of two colors (e.g., hex codes) used to
  differentiate the groups in the plot. Defaults to \code{c("#FF5733", "#33FFCE")}.}
}

\details{
When \code{plot_centrality_by_group} is invoked:

\enumerate{
  \item The function loads \pkg{dplyr}, \pkg{ggplot2}, and \pkg{forcats}.
  \item It calls \code{centralityTable()} on the two network graphs in \code{networks_groups}.
  \item The resulting table is cleaned, renamed, and filtered to retain only the specified
  \code{measure_spec}.
  \item The order of nodes along the y-axis is determined by their mean centrality value across
  groups, using \code{forcats::fct_reorder()}.
  \item A \pkg{ggplot2} visualization is generated, showing:
    \itemize{
      \item Lines connecting centrality scores across groups for each node.
      \item Points indicating each group’s z-score per node, with distinct colors and shapes.
      \item Axes flipped for readability (\code{coord_flip()}).
    }
  \item The function assembles and returns a named list containing:
    \itemize{
      \item \code{$plot}: the \pkg{ggplot2} object ready for display or saving.
      \item \code{$table}: the formatted data frame used for plotting, ready for export.
    }
}
}

\value{
A named list with two elements:
\itemize{
  \item \code{$plot}: A \pkg{ggplot2} object showing node-level centrality by group.
  \item \code{$table}: A tidy \pkg{tibble} containing node names, group labels, z-scores, and the
  selected centrality measure.
}
}

\examples{
\dontrun{
library(EGAnet)
library(ggplot2)

# Example: two group-specific networks
data("wmt2") # Working memory test data
net1 <- EGA(wmt2[1:100, ])   # Group 1
net2 <- EGA(wmt2[101:200, ]) # Group 2

# Compare centrality Strength between groups
result <- plot_centrality_by_group(
  networks_groups = list(net1, net2),
  replacements = c("Group A", "Group B"),
  measure_spec = "Strength",
  color_palette = c("#E41A1C", "#377EB8")
)

# Display results
result$plot   # ggplot2 figure
result$table  # formatted data
}
}

\author{
Dr. José Ventura-León
}
