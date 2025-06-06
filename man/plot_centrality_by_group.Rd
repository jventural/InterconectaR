\name{plot_networks_by_group}
\alias{plot_networks_by_group}
\title{Plot Network Structures for Multiple Groups}
\description{
The \code{plot_networks_by_group} function takes a named list of network objects (each containing at least a \code{graph} element compatible with \pkg{qgraph}) and produces a combined visualization of these network structures arranged in a grid. For each group, it (1) computes a common layout based on the first network, (2) generates an individual \pkg{qgraph} plot (optionally with pie charts, custom labels, and grouping colors), (3) renders each plot to a temporary PNG file, then (4) reads that image back into R as a raster and wraps it into a \pkg{ggplot2} object. Finally, it assembles all group-specific rasters into a single multi-panel figure via \pkg{patchwork}, respecting the specified number of columns (\code{ncol}) or automatically determining it from the total number of groups.
}

\usage{
plot_networks_by_group(networks_by_group,
width = 7,
height = 7,
units = "in",
res = 300,
groups = list("Factor1" = 1:6, "Factor2" = 7:12),
labels = NULL,
pie = NULL,
legend.cex = 0.5,
GLratio = 1.5,
ncol = NULL,
plot_margin = 20,
title_spacing = 6,
show_plot = TRUE)
}

\arguments{
\item{\code{networks_by_group}}{A named list of network objects. Each element must be a list that contains at least:
\itemize{
\item \code{$graph}: a matrix, data frame, or \pkg{qgraph}-compatible object representing the adjacency/weight structure of the network.
\item \code{$labels}: (optional) a character vector of node labels for use in \pkg{qgraph}. If \code{labels = NULL}, the function will attempt to use \code{networks_by_group[[i]][["labels"]]} internally.}
}
\item{\code{width}}{Numeric. Width of the temporary PNG device (in the units specified by \code{units}). Default is \code{7}.}
\item{\code{height}}{Numeric. Height of the temporary PNG device (in the units specified by \code{units}). Default is \code{7}.}
\item{\code{units}}{Character. Units for \code{width} and \code{height}, passed to \code{\link[grDevices]{png}()}. Typical values are \code{"in"}, \code{"cm"}, or \code{"px"}. Default is \code{"in"}.}
\item{\code{res}}{Numeric. Resolution (in dots per inch) for the temporary PNG device. Default is \code{300}.}
\item{\code{groups}}{A named list where each element defines a grouping of node indices for color/shape assignment in \pkg{qgraph}. For example, \code{list("Factor1" = 1:6, "Factor2" = 7:12)}. Default grouping is two blocks of six nodes each.}
\item{\code{labels}}{Optional. A character vector of length equal to the number of nodes, providing custom node labels for all networks. If \code{NULL}, the function will use \code{networks_by_group[[i]][["labels"]]} (if available) for each group.}
\item{\code{pie}}{Optional. A named list of numeric matrices (or data frames) representing pie-chart values for each node in each group. The names must match \code{names(networks_by_group)}. If supplied, \pkg{qgraph} will display pies at each node. Default is \code{NULL}.}
\item{\code{legend.cex}}{Numeric. Scaling factor for the legend text in \pkg{qgraph}. Default is \code{0.5}.}
\item{\code{GLratio}}{Numeric. Ratio of graph area to legend area passed to \pkg{qgraph}'s \code{GLratio} argument. Default is \code{1.5}.}
\item{\code{ncol}}{Integer or \code{NULL}. Number of columns in the final combined plot produced by \pkg{patchwork}. If \code{NULL}, the function computes \code{ceiling(sqrt(n_groups))}, where \code{n_groups = length(networks_by_group)}.}
\item{\code{plot_margin}}{Numeric. Margin (in points) around each raster image inserted into the \pkg{ggplot2} panel. This value is used in \code{\link[ggplot2]{theme}()} as \code{plot.margin}. Default is \code{20}.}
\item{\code{title_spacing}}{Numeric. Amount of top margin (in lines) reserved for the title in each \pkg{qgraph} panel. This is passed to \code{\link[graphics]{par}(mar = c(2,2,title_spacing,2))}. Default is \code{6}.}
\item{\code{show_plot}}{Logical. If \code{TRUE}, the combined figure is printed to the current graphics device; if \code{FALSE}, it is returned invisibly without printing. Default is \code{TRUE}.}
}

\details{
\strong{Workflow}:
\enumerate{
\item The function first loads (or assumes available) the required packages: \pkg{ggplot2}, \pkg{patchwork}, \pkg{magick}, and \pkg{grid}.
\item It computes a single “average layout” (\code{L}) for all networks by calling \code{averageLayout(...)} on the list of adjacency matrices (\code{$graph}) in \code{networks_by_group}. This ensures that all panels share the same node placement.
\item For each group (\code{i} in \code{seq_along(networks_by_group)}):
\enumerate{
\item A temporary PNG device is opened with dimensions \code{width} × \code{height} at \code{res} DPI.
\item \pkg{qgraph} is called with:
\itemize{
\item \code{layout = L} (the shared layout).
\item \code{palette = "ggplot2"} so node and edge colors follow \pkg{ggplot2} conventions.
\item \code{groups = groups}, which assigns node indices to named groups for coloring.
\item \code{labels} either from the global \code{labels} argument or \code{networks_by_group[[i]][["labels"]]}.
\item If \code{pie} is supplied and \code{pie[[group_name]]} exists, that matrix is passed to \code{pie = pie_values}.
\item A title is drawn above each plot via \code{title = paste0("\n", "Group: ", group_name)}, with \code{title.cex = 1.6}.
\item \code{edge.labels = TRUE}, \code{edge.label.cex = 1.5}, \code{edge.label.position = 0.5}, \code{border.width = 2.5}, \code{label.cex = 1}, and \code{legend.cex = legend.cex}.
\item \code{legend.mode = "groups"}, \code{GLratio = GLratio}, \code{vsize = 12}, \code{curveAll = 2}, \code{minimum = 0.10}, \code{mar = c(5, 3, 5, 3)}, and \code{rescale = TRUE}.
}
\item The device is turned off, producing a PNG file at \code{temp_file}.
\item That temporary PNG is read back into R via \code{magick::image_read()}, coerced to a raster with \code{as.raster()}, and wrapped into a simple \pkg{ggplot2} object (\code{p}) that displays the raster in \code{annotation_raster(...)} over the [0,1] × [0,1] coordinate space, with \code{theme_void()} plus a \code{plot.margin} set to \code{margin(plot_margin, plot_margin, plot_margin, plot_margin)}.
\item \code{p} is stored in \code{plot_list[[i]]} and the temporary file is deleted.
}
\item After all groups are processed, if \code{ncol} was \code{NULL}, it is recalculated as \code{ceiling(sqrt(n_groups))}.
\item If there is only one group, \code{combined_plot} is just that single \code{ggplot2} object; otherwise, \code{wrap_plots(plot_list, ncol = ncol)} arranges them into a grid.
\item If \code{show_plot = TRUE}, \code{print(combined_plot)} is called.
\item The function returns the \code{combined_plot} (invisibly if \code{show_plot = FALSE}).
}
}

\value{
A \pkg{ggplot2} object representing the combined raster-based network plots for each group. Each panel contains the original \pkg{qgraph} visualization (rendered as a PNG and then treated as a raster), preserving all edge labels, pie charts (if provided), group colorings, and titles. This object can be further modified (e.g., resized, annotated) since it is a \pkg{ggplot2} object constructed with \code{annotation_raster()} calls.
}

\examples{
\dontrun{
library(qgraph)

# Simulate two small adjacency matrices for demonstration
set.seed(42)
mat1 <- matrix(runif(25, 0, 1), nrow = 5)
mat2 <- matrix(runif(25, 0, 1), nrow = 5)

# Create two “network” objects with $graph and optional $labels
network1 <- list(graph = mat1, labels = paste0("Node", 1:5))
network2 <- list(graph = mat2, labels = paste0("Node", 1:5))

# networks_by_group <- list(
"SampleGroupA" = network1,
"SampleGroupB" = network2
)

# Optionally define a simple grouping (e.g., first 2 nodes vs. last 3)
groups_list <- list(
"Subset1" = 1:2,
"Subset2" = 3:5
)

# Optionally define pie charts for each node in each group
Here, each node has two slices summing to 1
pieA <- matrix(runif(10, 0.2, 0.8), nrow = 5, ncol = 2)
pieB <- matrix(runif(10, 0.1, 0.9), nrow = 5, ncol = 2)
pie_values <- list("SampleGroupA" = pieA, "SampleGroupB" = pieB)

# Generate and display the combined figure (2 columns automatically)
combined <- plot_networks_by_group(
networks_by_group = networks_by_group,
width = 5, height = 5, units = "in", res = 200,
groups = groups_list,
labels = NULL,
pie = pie_values,
legend.cex = 0.6,
GLratio = 2,
ncol = NULL,
plot_margin = 10,
title_spacing = 4,
show_plot = TRUE
)

# You can further customize the returned ggplot object:
combined + ggplot2::ggtitle("Combined Network Structures Across Groups")
}
}

\author{
Dr. José Ventura-León
}
