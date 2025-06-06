\name{plot_networks_by_group}
\alias{plot_networks_by_group}
\title{Plot Multiple Network Structures Side by Side by Group}

\description{
The \code{plot_networks_by_group} function generates a combined visualization of multiple network objects, each rendered via \code{qgraph}, arranged in a grid layout. For each group, the function (1) computes a shared node layout across all networks, (2) renders each individual network to a temporary PNG with \code{qgraph}, (3) reads each PNG back into R as a raster, and (4) wraps each raster into a \pkg{ggplot2} panel. Finally, it uses \pkg{patchwork} to assemble all panels into a single multi‐column figure. This approach preserves \code{qgraph}’s native styling (edge labels, node pies, group coloring, etc.) while returning a \pkg{ggplot2} object that can be further customized or saved with \code{ggsave()}.

\strong{Key Features}:
\itemize{
\item Computes an average layout across all supplied network adjacency matrices so that nodes occupy consistent positions in every panel.
\item Supports node‐level pie charts (\code{pie} argument), custom node labels, and group‐color assignments via \code{groups}.
\item Uses temporary PNG files and \code{magick::image_read()} to rasterize each \code{qgraph} plot, then embeds these rasters in \pkg{ggplot2} panels for final composition.
\item Automatically handles the number of columns (\code{ncol}) by defaulting to \code{ceiling(sqrt(n_groups))} if not specified, ensuring a near‐square arrangement of panels.
\item Returns a single \pkg{ggplot2} object, allowing downstream annotation, scaling, or saving with \code{ggsave()}.
}
}

\usage{
plot_networks_by_group(
networks_by_group,
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
show_plot = TRUE
)
}

\arguments{
\item{\code{networks_by_group}}{A named list of network objects. Each element must be a list containing at least:
\itemize{
\item \code{$graph}: a square numeric adjacency matrix (or \pkg{qgraph}‐compatible object) representing edge weights for that group.
\item \code{$labels} (optional): a character vector of node labels (length equal to the number of rows/columns of \code{$graph}). If \code{labels = NULL}, the function will automatically attempt to use \code{networks_by_group[[i]][["labels"]]} internally.}
}
\item{\code{width}}{Numeric. Width (in the units specified by \code{units}) of each temporary PNG device used for \code{qgraph} rendering. Default is \code{7}.}
\item{\code{height}}{Numeric. Height (in the units specified by \code{units}) of each temporary PNG device. Default is \code{7}.}
\item{\code{units}}{Character. Units for \code{width} and \code{height} passed to \code{\link[grDevices]{png}()}. Common values are \code{"in"}, \code{"cm"}, or \code{"px"}. Default is \code{"in"}.}
\item{\code{res}}{Numeric. Resolution in dots per inch (DPI) for the PNG device. Higher values produce finer detail. Default is \code{300}.}
\item{\code{groups}}{A named list of integer vectors indicating node‐group assignments for color/shape in \code{qgraph}. For example, \code{list("Factor1" = 1:6, "Factor2" = 7:12)} will color nodes 1–6 as “Factor1” and nodes 7–12 as “Factor2.”}
\item{\code{labels}}{Optional. A character vector of custom node labels to apply to all networks (length must equal the number of nodes). If \code{NULL}, the function uses \code{networks_by_group[[i]][["labels"]]} for each panel.}
\item{\code{pie}}{Optional. A named list of numeric matrices (or data frames) providing pie‐chart values at each node. Each element must be named exactly as one of \code{names(networks_by_group)}. If provided, \code{qgraph} will draw pie charts at each node. Default is \code{NULL}.}
\item{\code{legend.cex}}{Numeric. Scaling factor for the \code{qgraph} legend text. Default is \code{0.5}.}
\item{\code{GLratio}}{Numeric. Ratio of graph‐area to legend‐area passed to \code{qgraph}'s \code{GLratio} argument. Default is \code{1.5}.}
\item{\code{ncol}}{Integer or \code{NULL}. Number of columns in the final composite layout (via \pkg{patchwork}). If \code{NULL}, the function computes \code{ncol = ceiling(sqrt(n_groups))}, where \code{n_groups = length(networks_by_group)}.}
\item{\code{plot_margin}}{Numeric. Margin (in points) to place around each raster panel within \pkg{ggplot2}. This is passed to \code{\link[ggplot2]{theme}()} as \code{plot.margin = margin(plot_margin, plot_margin, plot_margin, plot_margin)}. Default is \code{20}.}
\item{\code{title_spacing}}{Numeric. Top margin (in lines) allocated for the title in each \code{qgraph} panel. Passed to \code{\link[graphics]{par}(mar = c(2, 2, title_spacing, 2))}. Default is \code{6}.}
\item{\code{show_plot}}{Logical. If \code{TRUE}, the function will immediately print the combined \pkg{ggplot2} object to the current graphics device. If \code{FALSE}, it returns the object invisibly without printing. Default is \code{TRUE}.}
}

\details{
When \code{plot_networks_by_group} is invoked:
\enumerate{
\item The function loads the required packages: \pkg{ggplot2}, \pkg{patchwork}, \pkg{magick}, and \pkg{grid}. It assumes \pkg{qgraph} is already loaded by the user or will generate an error if not.
\item It computes a shared node layout \code{L} by calling \code{averageLayout(...)} on the list of adjacency matrices \code{lapply(networks_by_group, function(x) x$graph)}. This ensures that nodes appear in the same positions across all panels.
\item For each group index \code{i} in \seq_along(networks_by_group)}:
\itemize{
\item Determine \code{group_name <- names(networks_by_group)[i]}.
\item If \code{pie} is non‐\code{NULL} and \code{group_name} exists in \code{names(pie)}, set \code{pie_values <- pie[[group_name]]}; otherwise, \code{pie_values <- NULL}.
\item Create a temporary PNG file \code{temp_file <- tempfile(fileext = ".png")}.
\item Open a PNG device with \code{png(filename = temp_file, width = width, height = height, units = units, res = res)}.
\item Adjust outer margins via \code{par(mar = c(2, 2, title_spacing, 2))}, leaving room for the title.
\item Call \code{qgraph(networks_by_group[[i]]$graph, layout = L, palette = "ggplot2", groups = groups, labels = if (is.null(labels)) networks_by_group[[i]][["labels"]] else labels, pie = pie_values, title = paste0("\n","Group: ", group_name), title.cex = 1.6, edge.labels = TRUE, edge.label.cex = 1.5, edge.label.position = 0.5, border.width = 2.5, label.cex = 1, legend.cex = legend.cex, legend.mode = "groups", GLratio = GLratio, vsize = 12, curveAll = 2, minimum = 0.10, mar = c(5, 3, 5, 3), rescale = TRUE)} to render the network with:
\itemize{
\item \code{layout = L} ensuring consistent node positions.
\item \code{palette = "ggplot2"} so that \code{qgraph} uses \pkg{ggplot2} colors.
\item Node / pie settings as specified by \code{pie_values}, \code{groups}, \code{labels}, etc.
\item A title indicating the group name, with a leading newline so the title sits slightly below the top margin.
}
\item Close the PNG device \code{dev.off()}, generating \code{temp_file}.
\item Read \code{temp_file} back into R as an Image object via \code{magick::image_read(temp_file)}, convert to an R raster with \code{as.raster()}, then wrap into a \code{grid::rasterGrob()}.
\item Create a \pkg{ggplot2} object \code{p <- ggplot() + annotation_raster(img_raster, xmin = 0, xmax = 1, ymin = 0, ymax = 1) + xlim(0, 1) + ylim(0, 1) + theme_void() + theme(plot.margin = margin(plot_margin, plot_margin, plot_margin, plot_margin))}. This places the raster as a full‐panel background with no axis or legend.
\item Append \code{p} to \code{plot_list[[i]]} and delete \code{temp_file}.
}
\item After processing all groups, if \code{ncol} is \code{NULL}, compute \code{ncol <- ceiling(sqrt(n_groups))} to arrange panels in a roughly square grid.
\item If there is exactly one element in \code{plot_list}, set \code{combined_plot <- plot_list[[1]]}. Otherwise, call \code{combined_plot <- wrap_plots(plot_list, ncol = ncol)} to assemble the layout.
\item If \code{show_plot = TRUE}, print \code{combined_plot}.
\item Return \code{combined_plot} (invisibly if \code{show_plot = FALSE}).
}
}

\value{
A \pkg{ggplot2} object containing the assembled network panels as rasters. Each panel corresponds to one element of \code{networks_by_group}, showing the \code{qgraph} visualization of that group’s adjacency structure. Nodes are consistently placed across panels, and optional pie charts, custom labels, and group colorings are preserved. Because the result is a \pkg{ggplot2} object, you can further annotate it or save it with \code{\link[ggplot2]{ggsave}()}.
}

\examples{
\dontrun{
library(qgraph)
library(ggplot2)

# 1. Simulate three small adjacency matrices for demonstration
set.seed(2025)
matA <- matrix(runif(25, 0, 1), nrow = 5)
matB <- matrix(runif(25, 0, 1), nrow = 5)
matC <- matrix(runif(25, 0, 1), nrow = 5)

# 2. Create “network” objects with $graph and $labels
networkA <- list(graph = matA, labels = paste0("N", 1:5))
networkB <- list(graph = matB, labels = paste0("N", 1:5))
networkC <- list(graph = matC, labels = paste0("N", 1:5))

networks_by_group <- list(
GroupA = networkA,
GroupB = networkB,
GroupC = networkC
)

# 3. Optionally define node grouping (e.g., first two vs. last three)
groups_list <- list(
Subset1 = 1:2,
Subset2 = 3:5
)

# 4. Optionally define pie values for two groups
pieA <- matrix(runif(10, 0.2, 0.8), nrow = 5, ncol = 2)
pieB <- matrix(runif(10, 0.1, 0.9), nrow = 5, ncol = 2)
pie_values <- list(GroupA = pieA, GroupB = pieB)

# 5. Generate and display the combined figure (2 columns by default)
combined_plot <- plot_networks_by_group(
networks_by_group = networks_by_group,
width = 5,
height = 5,
units = "in",
res = 200,
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

# 6. Save the combined plot (e.g., 10" × 4" at 300 dpi)
ggsave(
filename = "multiple_networks_by_group.png",
plot = combined_plot,
width = 10,
height = 4,
units = "in",
dpi = 300
)
}
}

\author{
Dr. José Ventura-León
}
