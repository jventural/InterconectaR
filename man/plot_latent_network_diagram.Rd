\name{plot_latent_network_diagram}
\alias{plot_latent_network_diagram}
\title{Plot Latent and Manifest Variables as a Network Diagram}
\description{
The \code{plot_latent_network_diagram} function generates and saves a customizable network-style path diagram of latent variables extracted from a structural equation model (SEM) object (from \pkg{lavaan} or \pkg{psychonetrics}).
Both latent and manifest variables can be displayed, with flexible options to control node size, labels, colors, layout, edge styles, and device resolution.
The function automatically fills missing labels and colors, and prevents manifest nodes from being hidden unintentionally when \code{show_manifest = TRUE}.
}
\usage{
plot_latent_network_diagram(
  model_object,
  filename,
  output_path = ".",
  width = 9.5,
  height = 6.5,
  resolution = 1500,
  node_labels = NULL,
  color_scheme = NULL,
  color_palette = "auto",
  plot_settings = NULL,
  device_settings = NULL,
  edge_settings = NULL,
  save_message = TRUE,
  show_manifest = TRUE
)
}
\arguments{
\item{\code{model_object}}{An SEM fit object, either a \code{lavaan} object or a \code{psychonetrics} object.}
\item{\code{filename}}{Character. Name of the file (with extension: “.png”, “.jpg”, or “.pdf”) to which the diagram will be written.}
\item{\code{output_path}}{Character. Directory in which to save the file. Defaults to the current working directory (".").}
\item{\code{width}}{Numeric. Width of the graphics device in inches. Default is 9.5.}
\item{\code{height}}{Numeric. Height of the graphics device in inches. Default is 6.5.}
\item{\code{resolution}}{Integer. Resolution (dots per inch) for raster formats (“png”, “jpg”). Default is 1500.}
\item{\code{node_labels}}{Optional character vector of node labels. If \code{NULL}, default labels are generated from the model. If length differs from the total number of manifest + latent variables, defaults are added automatically.}
\item{\code{color_scheme}}{Optional list with elements \code{man} and \code{lat} specifying colors for manifest and latent nodes. If only one of them is supplied, the other is auto-filled.}
\item{\code{color_palette}}{Character. Predefined palette name (“auto”, “brewer_set1”, “viridis”, “rainbow”, etc.) passed to the internal \code{generate_color_scheme} function. Default is “auto”.}
\item{\code{plot_settings}}{List of graphical settings passed to \code{\link[semPlot]{semPaths}} (e.g. \code{layout}, \code{curve}, \code{sizeLat}, \code{sizeMan}, \code{manLatDist}, \code{label.cex}). Defaults are provided and patched internally to avoid hiding manifest nodes when \code{show_manifest = TRUE}.}
\item{\code{device_settings}}{List with device parameters \code{width}, \code{height}, \code{units}, and \code{res} for the graphics device. Overrides \code{width}, \code{height}, \code{resolution} if non-\code{NULL}.}
\item{\code{edge_settings}}{List with edge settings such as \code{edge.label.cex}, \code{edge.label.color}, \code{edge.label.position}, and \code{edge.width}. Defaults are modest, but user can control placement and size to avoid overlapping labels.}
\item{\code{save_message}}{Logical. If \code{TRUE}, prints a message to the console indicating where the plot was saved.}
\item{\code{show_manifest}}{Logical. If \code{TRUE}, displays both manifest and latent variables; if \code{FALSE}, hides manifest nodes and only shows latent variables.}
}
\details{
\enumerate{
\item Extracts standardized factor loadings (\code{lambda}), latent covariances (\code{psi}), and residual variances (\code{theta}) from \code{model_object}.
\item Builds a LISREL-style model with \code{\link[semPlot]{lisrelModel}}.
\item Automatically generates node labels and colors when not supplied, ensuring lengths match the number of nodes.
\item Ensures that when \code{show_manifest = TRUE}, manifest variables are visible even if \code{sizeMan = 0} is accidentally passed.
\item Provides fine-grained control over node sizes (latent vs manifest), text scaling, layout, and distances (e.g., \code{manLatDist} to separate manifest from latent).
\item Allows adjustment of arrow curvature, thickness, and label placement to improve readability and reduce overlaps.
\item The output format is inferred from \code{filename}; unsupported formats raise an error.
\item The graphics device is closed properly after plotting, and an optional console message confirms the save location.
}
}
\value{
(invisible) \code{NULL}.
Side effect: saves the network diagram to \code{file.path(output_path, filename)}.
}
\examples{
plot_latent_network_diagram(
  model_object = lnm,
  filename = "Figura3.png",
  output_path = "Figuras",
  node_labels = c("Dei", "Emo", "Sex", "Dsi", "Sat"),
  color_palette = "brewer_set2",
  plot_settings = list(
    sizeLat = 15,             # Larger latent circles
    sizeMan = 8,              # Bigger manifest rectangles
    label.cex = 1.2,          # Larger text labels
    manLatDist = 40,          # More distance between latent and manifests
    curve = 2.0               # Stronger curvature for latent-latent paths
  ),
  edge_settings = list(
    edge.label.cex = 0.8,     # Smaller path labels
    edge.label.position = 0.35,
    edge.width = 2,
    edge.label.color = "grey32"
  ),
  show_manifest = TRUE
)
}
\author{
Dr. José Ventura-León
}
