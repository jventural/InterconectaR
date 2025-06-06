\name{combine_groupBy}
\alias{combine_groupBy}
\title{Combine Network and Centrality Visualizations by Group}

\description{
The \code{combine_groupBy} function merges a network visualization and its associated centrality (and optionally bridge centrality) plots into a single layout for a given group. This is particularly useful for side-by-side comparison of network structure with node-based metrics. The combined figure features three labeled sections (“A”, “B”, and “C”) and allows precise control over relative widths, margins, and rendering quality.

\strong{Key Features}:
\itemize{
\item Integrates a network plot (e.g., generated via \code{qgraph}) with one or two \pkg{ggplot2} plots (centrality and bridge metrics).
\item Assigns clear section labels: “A” for the network, “B” for the centrality plot, and “C” for the bridge metric plot.
\item Dynamically adjusts the layout depending on whether \code{bridge_plot_group} is provided.
\item Allows customization of column widths, overall rendering resolution, and whether to display or only return the combined plot.
\item Automatically converts input objects (\code{magick-image}, \code{ggplot}, or \code{grob}) into grid-compatible grobs or rasters for seamless composition.
}
}

\usage{
combine_groupBy(
red_group,
plot_centralidad_group,
bridge_plot_group = NULL,
width_a = 8,
width_bc = 4.5,
show_plot = TRUE
)
}

\arguments{
\item{\code{red_group}}{A graphic object representing the group’s network structure. It can be one of:
\itemize{
\item A \code{magick-image}, (e.g., created with \code{magick::image_graph}).
\item A \code{ggplot} object.
\item A \code{grob}, \code{gtable}, or \code{zeroGrob}.
}
Internally, if \code{red_group} is a \code{magick-image}, it is written to a temporary PNG and read back as a \code{rasterGrob}; if it is a \code{ggplot}, it is converted with \code{ggplotify::as.grob()}; if already a grob, it is used directly.}
\item{\code{plot_centralidad_group}}{A \pkg{ggplot2} object showing node centrality metrics (e.g., bar or line plot of z-scores for Strength, Closeness, Betweenness, etc.). This section is labeled “B” in the combined layout.}
\item{\code{bridge_plot_group}}{(Optional) A \pkg{ggplot2} object representing bridge centrality (e.g., Bridge Expected Influence). This section is labeled “C”. If \code{NULL}, the “C” section is left empty, and only “B” appears in the right column. Default is \code{NULL}.}
\item{\code{width_a}}{Numeric. Width in inches allocated to the left column (the network plot). Default: \code{8}.}
\item{\code{width_bc}}{Numeric. Width in inches allocated to the right column. If \code{bridge_plot_group = NULL}, this entire width is used by the “B” plot; if \code{bridge_plot_group} is not \code{NULL}, the right column is split vertically into two equal portions (“B” above “C”), both sharing \code{width_bc}. Default: \code{4.5}.}
\item{\code{show_plot}}{Logical. If \code{TRUE}, the combined figure is printed to the active graphics device. If \code{FALSE}, the combined \pkg{ggplot2} object is returned invisibly without printing. Default: \code{TRUE}.}
}

\details{
\strong{Internal Workflow}:
\enumerate{
\item \strong{Load or install required packages}: The function ensures that \pkg{ggplot2}, \pkg{gridExtra}, \pkg{grid}, \pkg{cowplot}, \pkg{ggplotify}, \pkg{magick}, and \pkg{png} are installed and loaded.
\item \strong{Process \code{red_group}}:
\itemize{
\item If \code{red_group} is a \code{magick-image}, write it to a temporary PNG, read it back with \code{png::readPNG()}, and convert to \code{rasterGrob} for grid placement.
\item If it is a \code{ggplot} object, convert with \code{ggplotify::as.grob()}.
\item If it already inherits from \code{grob} or \code{gtable}, use it unchanged.
\item Otherwise, throw an error indicating that \code{red_group} must be one of the supported types.
}
\item \strong{Generate section labels}: Create three \pkg{cowplot}-based labels “A”, “B”, and “C” with bold fontface and size 14, positioned at the left margin of each respective section.
\item \strong{Prepare subplots “B” and “C”}:
\itemize{
\item Wrap \code{plot_centralidad_group} in a \pkg{cowplot::ggdraw()} and add a 5-point margin on all sides.
\item If \code{bridge_plot_group} is provided, wrap it similarly; otherwise, use an empty \code{theme_void()} grob for “C”.
}
\item \strong{Construct the right column}:
\itemize{
\item If \code{bridge_plot_group = NULL}, stack “B” only, placing the “B” label above its plot with heights \code{c(0.1, 0.9)}.
\item If \code{bridge_plot_group} is not \code{NULL}, stack “B” and “C” vertically, each with a label and their plot content, each occupying 50% of the right column’s vertical space (\code{heights = c(0.5, 0.5)}).
\item Both cases use \code{gridExtra::arrangeGrob()} to build the right column. The relative widths of left vs. right columns are set to \code{c(width_a, width_bc)}.
}
\item \strong{Construct the left column}:
\itemize{
\item Wrap the processed network grob into a \code{grid::gTree()} with a viewport of width and height \code{0.99 "npc"} (i.e., 99% of cell size) so the network occupies nearly the entire cell.
\item Stack the “A” label (5% of height) above the network grob (95% of height) via \code{gridExtra::arrangeGrob()}.
}
\item \strong{Combine left and right columns}:
\itemize{
\item Use \code{gridExtra::arrangeGrob(..., ncol = 2, widths = c(width_a, width_bc))} to arrange the left and right columns side by side with specified proportion.
}
\item \strong{Convert to \pkg{ggplot2} object}: Apply \code{ggplotify::as.ggplot()} to the assembled \code{gtable}, yielding a \pkg{ggplot2}-compatible object that can be saved via \code{ggsave()} or further annotated.
\item \strong{Print or return}: If \code{show_plot = TRUE}, print the \pkg{ggplot2} object. In all cases, return the \pkg{ggplot2} object (invisibly if not printed).
}
}

\value{
A \pkg{ggplot2} object representing the combined figure with three labeled sections:
\itemize{
\item \textbf{A:} The network visualization (as a raster or grob) filling nearly the full area of its left column cell.
\item \textbf{B:} The centrality plot (wrapped in a margin and labeled “B”).
\item \textbf{C:} (Optional) The bridge centrality plot (wrapped and labeled “C”); if \code{bridge_plot_group = NULL}, this section remains empty.
}
This object can be passed directly to \code{\link[ggplot2]{ggsave}()} or modified with standard \pkg{ggplot2} additions.
}

\examples{
\dontrun{
library(qgraph)
library(ggplot2)

# 1. Create an example network using qgraph (but do not plot directly)
set.seed(123)
adjacency <- matrix(runif(100, 0, 1), nrow = 10)

The “DoNotPlot = TRUE” flag instructs qgraph to return the object without rendering.
red_group <- qgraph::qgraph(adjacency, layout = "spring", DoNotPlot = TRUE)

# 2. Build example data for centrality and bridge metrics
data_example <- data.frame(
Node = paste0("N", 1:10),
CentralityValue = rnorm(10),
BridgeValue = runif(10, 0, 1)
)

# Centrality bar plot (“B”)
plot_centralidad_group <- ggplot(data_example, aes(x = reorder(Node, CentralityValue), y = CentralityValue)) +
geom_col(fill = "steelblue") +
coord_flip() +
labs(x = "Node", y = "Centrality (z-score)") +
theme_minimal()

# Bridge centrality point plot (“C”)
bridge_plot_group <- ggplot(data_example, aes(x = reorder(Node, BridgeValue), y = BridgeValue)) +
geom_point(color = "tomato", size = 3) +
coord_flip() +
labs(x = "Node", y = "Bridge Centrality") +
theme_minimal()

# 3. Combine network + centrality (no bridge plot)
combine_groupBy(
red_group = red_group,
plot_centralidad_group = plot_centralidad_group,
bridge_plot_group = NULL,
width_a = 7,
width_bc = 4,
show_plot = TRUE
)

# 4. Combine network + centrality + bridge
combine_groupBy(
red_group = red_group,
plot_centralidad_group = plot_centralidad_group,
bridge_plot_group = bridge_plot_group,
width_a = 7,
width_bc = 4,
show_plot = TRUE
)
}
}

\author{
Dr. José Ventura-León
}
