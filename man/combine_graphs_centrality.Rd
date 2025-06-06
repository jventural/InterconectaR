\name{combine_graphs_centrality}
\alias{combine_graphs_centrality}
\title{Combine Centrality Network and Comparative Plots into a Single ggplot Object}

\description{
The \code{combine_graphs_centrality} function generates a composite \pkg{ggplot2} object by embedding a centrality network graph (rendered via \code{qgraph}) as a raster alongside a separate comparative \pkg{ggplot2} visualization. Rather than writing the network out to disk permanently, it temporarily saves the \code{qgraph} output as a PNG, reads it back in, and places it into a blank \code{ggplot} canvas. The final object belongs to a custom class (\code{"silent_gg"}) that suppresses the typical “No shared levels found…” warning when printed or saved. Users can then further annotate, modify, or export the combined figure using standard \pkg{ggplot2} workflows (e.g., via \code{ggsave()}).

\strong{Key Features}:
\itemize{
\item Integrates a \code{qgraph}-generated centrality network (with node grouping, pie‐chart slices from \code{error_Model}, edge labels, custom layout, etc.) into a \code{ggplot2} canvas as a raster.
\item Places the network raster (\code{p1}) side‐by‐side with any user‐supplied \pkg{ggplot2} object (\code{Figura1_Derecha}) via \pkg{patchwork}, controlling the number of columns (\code{ncol}) and their relative widths (\code{widths}).
\item Automatically installs and loads required packages: \code{ggplot2}, \code{qgraph}, \code{png}, \code{grid}, \code{patchwork}, and \code{Cairo}.
\item Offers optional abbreviation of node labels to three characters via \code{abbreviate_labels}.
\item Wraps the combined object in class \code{"silent_gg"} and defines a \code{print.silent_gg} method that uses \code{suppressWarnings()}, thereby hiding the common \enquote{No shared levels found…} warning when rendering.
}
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
\item{\code{Figura1_Derecha}}{A \pkg{ggplot2} object representing the comparative plot (e.g., a bar chart, scatterplot, etc.) to place alongside the network raster.}
\item{\code{network}}{A list with at least two components:
\itemize{
\item \code{$graph}: a square numeric adjacency matrix suitable for \code{qgraph}.
\item \code{$labels}: a character vector of node labels (length must match the number of rows/columns of \code{graph}).
}
}
\item{\code{groups}}{A vector or list specifying node‐group membership for coloring in the \code{qgraph} visualization. Its length must equal the number of nodes (rows/columns) in \code{network$graph}.}
\item{\code{error_Model}}{A numeric vector (or matrix) of length (or dimension) matching the number of nodes. Each entry supplies values for pie‐chart slices at each node in the network.}
\item{\code{labels}}{Optional. A character vector of custom node labels. If \code{NULL}, \code{network$labels} is used.}
\item{\code{abbreviate_labels}}{Logical. If \code{TRUE}, node labels (from \code{labels} or \code{network$labels}) are truncated to their first three characters before plotting. Default is \code{FALSE}.}
\item{\code{ncol}}{Integer. Number of columns in the final combined layout (via \pkg{patchwork}). Default is \code{2}.}
\item{\code{widths}}{Numeric vector of length two specifying the relative widths of the network raster panel (\code{p1}) and \code{Figura1_Derecha}, respectively. Default is \code{c(0.50, 0.60)}.}
\item{\code{dpi}}{Integer. Resolution (dots per inch) for the temporary PNG output of the \code{qgraph} graph. Higher values yield a finer raster. Default is \code{600}.}
\item{\code{legend.cex}}{Numeric. Controls the legend text size in the \code{qgraph} output. Default is \code{0.1}.}
}

\details{
When \code{combine_graphs_centrality} is called:
\enumerate{
\item The function verifies that \pkg{ggplot2}, \pkg{qgraph}, \pkg{png}, \pkg{grid}, \pkg{patchwork}, and \pkg{Cairo} are installed; if any are missing, it installs and loads them.
\item If \code{abbreviate_labels = TRUE}, node labels are replaced by their first three characters.
\item A temporary PNG file (\code{tmp_file}) is created via \code{Cairo::CairoPNG()}, using the specified \code{dpi}, width = 1600 pixels, height = 1000 pixels.
\item \code{qgraph} renders the network:
\itemize{
\item \code{groups} defines node coloring.
\item \code{curveAll = 2}, \code{vsize = 18}, \code{esize = 18}, \code{palette = "pastel"}, \code{layout = "spring"} and other aesthetic parameters control node size, edge size, layout, edge labels, etc.
\item \code{labels = final_labels} supplies the node names.
\item \code{legend.cex} sets legend text scaling.
\item \code{pie = error_Model} draws pie charts at each node if \code{error_Model} is provided.
\item \code{layoutScale = c(0.9, 0.9)}, \code{GLratio = 2}, and \code{edge.label.cex = 1} further refine the layout and edge‐label appearance.
\item \code{details = FALSE} suppresses interactive details.
}
\item The PNG device is closed (\code{dev.off()}), producing \code{tmp_file}.
\item \code{png::readPNG(tmp_file)} reads the PNG into R as a raster array, which \code{grid::rasterGrob()} converts into a \code{grob} (\code{g1_raster}).
\item An empty \pkg{ggplot2} canvas (\code{p1}) is created, and \code{annotation_custom(g1_raster, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} places the network raster to fill the panel. \code{theme_void()} removes axes and margins.
\item \code{p1} (the rasterized network) and \code{Figura1_Derecha} are combined side‐by‐side using \code{patchwork::plot_layout(ncol = ncol, widths = widths)}. By default, \code{ncol = 2}, so \code{p1} occupies the first column and \code{Figura1_Derecha} occupies the second.
\item A custom class \code{"silent_gg"} is assigned to the combined \pkg{ggplot} object, and a \code{print.silent_gg} method is defined in the global environment to wrap \code{suppressWarnings(NextMethod())}, thereby hiding warnings such as “No shared levels found…”.
\item The temporary PNG file (\code{tmp_file}) is deleted.
\item The final combined object is returned. If \code{show_plot = TRUE}, it is printed immediately (with suppressed warnings). If \code{show_plot = FALSE}, it is returned invisibly without printing.
}
}

\value{
A \pkg{ggplot2} object of class \code{"silent_gg"} that contains:
\itemize{
\item The rasterized network visualization on the left.
\item The user‐provided comparative \pkg{ggplot2} object (\code{Figura1_Derecha}) on the right.
}
Printing or saving this object suppresses any “No shared levels found…” message. You can pass it directly to \code{ggsave()} or further modify it with additional \pkg{ggplot2} layers.
}

\examples{
\dontrun{
library(ggplot2)
library(qgraph)

# 1. Simulate an adjacency matrix and labels
set.seed(42)
adj_mat <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
diag(adj_mat) <- 0
network <- list(graph = adj_mat, labels = LETTERS[1:10])

# 2. Define node groups and error_Model (pie‐slice values)
groups <- rep(1:2, each = 5)
error_Model <- matrix(runif(20, 0, 1), nrow = 10, ncol = 2)

# 3. Create a comparative ggplot2 object (Figura1_Derecha)
data_pts <- data.frame(x = 1:10, y = rnorm(10))
Figura1_Derecha <- ggplot(data_pts, aes(x = x, y = y)) +
geom_point(size = 3) +
theme_minimal() +
labs(title = "Comparative Plot")

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

# 5. Save the combined plot (12" × 5" at 300 dpi)
ggsave(
filename = "combined_centrality_plot.png",
plot = combined_plot,
width = 12,
height = 5,
units = "in",
dpi = 300
)

# 6. Combine network + comparative plot with abbreviated labels
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

# 7. Display it
print(combined_plot_abbrev)
}
}

\author{
Dr. José Ventura-León
}
