\name{combine_graphs_centrality}
\alias{combine_graphs_centrality}
\title{Combine Centrality Network and Comparative Plots into a Single ggplot Object}

\description{
The \code{combine_graphs_centrality} function produces a unified \pkg{ggplot2} object by embedding a network‐centrality graph (generated via \code{qgraph}) as a raster image alongside a user‐provided comparative \pkg{ggplot2} plot. Internally, it renders the \code{qgraph} visualization to a temporary PNG (using \code{CairoPNG()}), reads that PNG back into R as a \code{rasterGrob}, and places it onto an otherwise blank \pkg{ggplot2} canvas. Meanwhile, the supplied \code{Figura1_Derecha} (e.g., a line‐and‐point plot) has its legend relabeled from “Métrica” to “Metric,” its legend items reordered to display one below the other (with labels “Bridge EI” and “EI”), and its legend positioned vertically on the right. Finally, both panels are arranged side‐by‐side via \pkg{patchwork}, with user‐defined column counts and relative widths. The resulting object inherits the custom class \code{"silent_gg"}, which suppresses the common “No shared levels found…” warning upon printing or saving.
}

\usage{
combine_graphs_centrality(
Figura1_Derecha,
network,
groups,
error_Model,
labels = NULL,
abbreviate_labels = FALSE,
ncol = 2,
widths = c(0.50, 0.60),
dpi = 600,
legend.cex = 0.1
)
}

\arguments{
\item{\code{Figura1_Derecha}}{A \pkg{ggplot2} object (e.g., a line‐and‐point plot) to be placed to the right of the network raster. Its legend will be modified—title changed to “Metric,” items relabeled as “Bridge EI” and “EI,” displayed vertically, and positioned on the right side of the plot.}
\item{\code{network}}{A list with at least two components:
\itemize{
\item \code{$graph}: A square numeric adjacency matrix suitable for \code{qgraph}.
\item \code{$labels}: A character vector of node labels, of length matching the number of rows/columns in \code{graph}.
}}
\item{\code{groups}}{A vector or list specifying group membership for each node (used by \code{qgraph} to color nodes). Must have length equal to the number of nodes in \code{network$graph}.}
\item{\code{error_Model}}{A numeric vector (or matrix) whose length (or row dimension) equals the number of nodes. Values are used to draw pie‐chart slices at each node in the network.}
\item{\code{labels}}{Optional. A character vector of custom node labels. If \code{NULL}, \code{network$labels} is used.}
\item{\code{abbreviate_labels}}{Logical. If \code{TRUE}, node labels (from \code{labels} or \code{network$labels}) are truncated to their first three characters before plotting. Default is \code{FALSE}.}
\item{\code{ncol}}{Integer. Number of columns in the final combined layout via \pkg{patchwork}. Default is \code{2}.}
\item{\code{widths}}{Numeric vector of length two specifying the relative widths of the network raster panel (left) and \code{Figura1_Derecha} (right). Default is \code{c(0.50, 0.60)}.}
\item{\code{dpi}}{Integer. Resolution (dots per inch) for the temporary PNG output of the \code{qgraph} graph. Higher values produce finer raster detail. Default is \code{600}.}
\item{\code{legend.cex}}{Numeric. Controls the text size of the legend in the \code{qgraph} output. Default is \code{0.1}.}
}

\details{
When \code{combine_graphs_centrality()} is invoked:
\enumerate{
\item Required packages (\pkg{ggplot2}, \pkg{qgraph}, \pkg{png}, \pkg{grid}, \pkg{patchwork}, and \pkg{Cairo}) are verified; any missing ones are installed and loaded automatically.
\item If \code{abbreviate_labels = TRUE}, the function truncates each node label to its first three characters via an internal \code{abbreviate_names()} helper.
\item A temporary PNG file (\code{tmp_file}) is created via \code{Cairo::CairoPNG()} with dimensions 1600×1000 pixels at the specified \code{dpi}.
\item \code{qgraph()} draws the network:
\itemize{
\item \code{groups} determines node coloring.
\item A spring layout (\code{layout = "spring"}) with curved edges (\code{curveAll = 2}).
\item Node size (\code{vsize = 18}), edge size (\code{esize = 18}), pastel palette, pie‐chart slices at nodes (\code{pie = error_Model}), and edge labels (\code{edge.labels = TRUE}, \code{edge.label.cex = 1}).
\item \code{labels = final_labels} supplies node names (abbreviated or original).
\item \code{legend.cex} sets scaling of the legend’s text for node‐group keys (if displayed).
\item \code{details = FALSE} suppresses interactive details.
\item \code{layoutScale = c(0.9, 0.9)} and \code{GLratio = 2} refine the layout.
}
\item The PNG device is closed (\code{dev.off()}), creating a static PNG file at \code{tmp_file}.
\item \code{png::readPNG(tmp_file)} reads the PNG into R as an array; \code{grid::rasterGrob()} converts it into a \code{grob} (\code{g1_raster}).
\item An empty \pkg{ggplot2} canvas (\code{p1}) is built and \code{annotation_custom(g1_raster, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} places the network raster to fill the entire panel. \code{theme_void()} removes axes, gridlines, and margins.
\item \code{Figura1_Derecha} (the supplied comparative plot) is modified:
\itemize{
\item \code{ggplot2::labs(color = "Metric", shape = "Metric")} changes the legend title from “Métrica” to “Metric.”
\item \code{ggplot2::scale_color_discrete(labels = c("Bridge EI", "EI"))} and \code{ggplot2::scale_shape_discrete(labels = c("Bridge EI", "EI"))} relabel the legend items.
\item \code{ggplot2::theme(legend.direction = "vertical", legend.position = "right", legend.box = "vertical")} forces a vertical legend on the right side of the plot, with one item per row.
}
\item \code{p1} and the modified \code{Figura1_Derecha} are combined side‐by‐side using \pkg{patchwork::plot_layout(ncol = ncol, widths = widths)}. By default, \code{ncol = 2}, so \code{p1} occupies the first column and \code{Figura1_Derecha} the second.
\item The combined \pkg{ggplot2} object is assigned class \code{"silent_gg"}. A custom \code{print.silent_gg} method is defined in the global environment to wrap \code{suppressWarnings(NextMethod())}, thereby silencing warnings such as “No shared levels found…” when rendering or saving.
\item The temporary PNG file (\code{tmp_file}) is deleted.
\item The final combined object is returned. Since it is of class \code{"silent_gg"}, printing or saving it will not emit common \code{ggplot2} legend warnings. It can be passed directly to \code{ggsave()} for export or further customized with additional \pkg{ggplot2} layers.
}
}

\value{
A \pkg{ggplot2} object of class \code{"silent_gg"} containing:
\itemize{
\item The \code{qgraph}‐generated network raster on the left panel.
\item The user‐supplied \code{Figura1_Derecha} (with updated legend title “Metric,” items “Bridge EI” and “EI” displayed vertically on the right) on the right panel.
}
Printing or saving this object will suppress any “No shared levels found…” message. Use \code{ggsave()} or further \pkg{ggplot2} modifications as needed.
}

\examples{
\dontrun{
library(ggplot2)
library(qgraph)

# 1. Create a random adjacency matrix and labels
set.seed(42)
adj_mat <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
diag(adj_mat) <- 0
network <- list(graph = adj_mat, labels = LETTERS[1:10])

# 2. Define node groups and error_Model (pie‐chart slice values)
groups <- rep(1:2, each = 5)
error_Model <- matrix(runif(20, 0, 1), nrow = 10, ncol = 2)

# 3. Build a comparative ggplot2 object with a line‐and‐point plot
data_pts <- data.frame(x = 1:10, y = rnorm(10))
Figura1_Derecha <- ggplot(data_pts, aes(x = x, y = y, color = factor(x %% 2), shape = factor(x %% 2))) +
geom_point(size = 3) +
geom_line() +
theme_minimal() +
labs(title = "Comparative Line & Point Plot", color = "Métrica", shape = "Métrica")

# 4. Combine network + comparative plot without label abbreviation
combined_plot <- combine_graphs_centrality(
Figura1_Derecha = Figura1_Derecha,
network = network,
groups = groups,
error_Model = error_Model,
labels = NULL,
abbreviate_labels = FALSE,
ncol = 2,
widths = c(0.5, 0.5),
dpi = 300,
legend.cex = 0.2
)

# 5. Save the combined figure (e.g., 12" × 5" at 300 dpi)
ggsave(
filename = "combined_centrality_plot.png",
plot = combined_plot,
width = 12,
height = 5,
units = "in",
dpi = 300
)

# 6. Combine network + comparative plot with abbreviated node labels
combined_plot_abbrev <- combine_graphs_centrality(
Figura1_Derecha = Figura1_Derecha,
network = network,
groups = groups,
error_Model = error_Model,
labels = NULL,
abbreviate_labels = TRUE,
ncol = 2,
widths = c(0.6, 0.4),
dpi = 600,
legend.cex = 0.1
)

# 7. Display the abbreviated‐label version
print(combined_plot_abbrev)
}
}

\author{
Dr. José Ventura-León
}
