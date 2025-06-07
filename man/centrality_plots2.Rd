\name{centrality_plots2}
\alias{centrality_plots2}
\title{Calculate and Visualize Centrality and Bridge Centrality Measures for Networks}

\description{
The \code{centrality_plots2} function computes and visualizes one or two node‐level centrality metrics for a given network: a standard centrality measure (e.g., \code{ExpectedInfluence} or \code{Strength}) and, optionally, a bridge centrality measure (e.g., \code{Bridge Expected Influence (1-step)}). The function returns a data frame of z‐scored values and a \code{ggplot2} object that plots nodes (either full names or three‐letter abbreviations) along the y‐axis, with points and connecting lines representing each selected metric on the x‐axis.
}

\usage{
centrality_plots2(
  qgraph_obj,
  network,
  groups = NULL,
  measure0 = "ExpectedInfluence",
  measure1 = NULL,
  color_palette = c("#FF0000", "#00A08A"),
  labels = NULL,
  legend_labels = NULL,
  use_abbrev = TRUE
)
}

\arguments{
\item{qgraph_obj}{An \code{igraph} or \code{qgraph} object representing the network (usually unthresholded adjacency). Required for computing bridge centrality via \code{networktools::bridge()}.}
\item{network}{A \code{qgraph} object representing the estimated network structure. Used to extract standard centrality metrics via \code{qgraph::centralityTable()}.}
\item{groups}{(Optional) A numeric or factor vector indicating community membership for each node in \code{qgraph_obj}. Passed to \code{networktools::bridge()} when \code{measure1} is not \code{NULL}. Defaults to \code{NULL}.}
\item{measure0}{Character string specifying the standard centrality metric to extract from \code{network}. Supported values: \code{"Strength"} or \code{"ExpectedInfluence"}. Defaults to \code{"ExpectedInfluence"}.}
\item{measure1}{Character string specifying the bridge centrality metric to compute from \code{qgraph_obj}. Supported values: \code{"Bridge Strength"}, \code{"Bridge Betweenness"}, \code{"Bridge Closeness"}, \code{"Bridge Expected Influence (1-step)"}, \code{"Bridge Expected Influence (2-step)"}, or \code{"communities"}. If \code{NULL}, only \code{measure0} is plotted. Defaults to \code{NULL}.}
\item{color_palette}{A length-2 character vector of colors (hex codes or named colors). \code{color_palette[1]} corresponds to \code{measure0} (or the single metric if \code{measure1} is \code{NULL}), and \code{color_palette[2]} corresponds to \code{measure1}, if provided. Defaults to \code{c("#FF0000", "#00A08A")}.}
\item{labels}{(Optional) A named character vector mapping original node names to custom labels. If provided, nodes whose names match \code{names(labels)} will be relabeled in the output table and plot. Defaults to \code{NULL}, in which case original node names are used.}
\item{legend_labels}{(Optional) A named character vector for custom legend text. The names should exactly match \code{c(measure1, measure0)}. If \code{NULL}, the function uses the measure names themselves as legend labels. Defaults to \code{NULL}.}
\item{use_abbrev}{Logical; if \code{TRUE}, nodes are shown on the y‐axis using a three‐letter abbreviation (first three letters of the first word, title‐cased). If \code{FALSE}, full node names are shown. Defaults to \code{TRUE}.}
}

\details{
This function performs the following workflow:
\enumerate{
\item
\strong{Install and load required packages (if missing):} \code{qgraph}, \code{dplyr}, \code{networktools}, \code{tibble}, \code{ggplot2}, \code{tidyr}, \code{stringr}, and \code{forcats}.
\item
\strong{Extract standard centrality (measure0):} Calls \code{qgraph::centralityTable(network)}, filters rows matching \code{measure0}, and retains two columns: \code{full_name = node} and \code{value}. Then it orders by \code{full_name}.
\item
\strong{Apply custom labels (if provided):} If \code{labels} is not \code{NULL}, replace any \code{full_name} that appears in \code{names(labels)} with \code{labels[full_name]}.
\item
\strong{Generate three‐letter abbreviations:} Creates a new column \code{Abrev} by taking the first three letters of the first word in \code{full_name}, converting to title case.
\item
\strong{If \code{measure1} is provided:}
  \begin{itemize}
    \item Compute bridge centrality: Calls \code{networktools::bridge(qgraph_obj, communities = groups, useCommunities = "all", normalize = FALSE)} to obtain a list, then extracts \code{b_obj[[measure1]]} (a named numeric vector of raw bridge values).
    \item Scale raw bridge values into z‐scores and store in column \code{measure1}.
    \item Merge with the standard centrality data by matching on \code{Abrev}. The resulting data frame (\code{cents2}) has columns \code{full_name}, \code{Abrev}, \code{measure0} (z‐scored), and \code{measure1} (z‐scored).
    \item Reshape to long format (\code{cents_long}) with columns \code{Abrev} (or \code{full_name} if \code{use_abbrev = FALSE}), \code{Measure}, and \code{Value}.
    \item Define a color palette mapping: if \code{legend_labels} is \code{NULL}, use \code{names(pal) = c(measure1, measure0)} and \code{pal = setNames(color_palette, c(measure1, measure0))}; otherwise use \code{legend_labels} to override.
    \item Build a \code{ggplot2} object (\code{Figura}) with \code{geom_point()} and \code{geom_line()}, mapping \code{x = z‐score}, \code{y = node (reordered by z‐score)}, \code{color = Measure}, and \code{group = Measure}. Axis labels and legend styles are set for readability.
    \item Assign class “\code{silent_gg}” to suppress any warnings when printing.
    \item Return a list with \code{table = cents2 \%>\% arrange(desc(measure0))} and \code{plot = Figura}.
  \end{itemize}
\item
\strong{If \code{measure1} is \code{NULL}:}
  \begin{itemize}
    \item Keep only columns \code{full_name}, \code{Abrev}, and \code{measure0} (renamed as \code{Value}), ordered by descending \code{Value}.
    \item Define a single‐color palette \code{pal_single = setNames(color_palette[1], measure0)} and \code{legend_lbls_single = setNames(measure0, measure0)}.
    \item Build a \code{ggplot2} object (\code{Figura}) with \code{geom_line(color = pal_single)} and \code{geom_point(color = pal_single)}, mapping \code{x = Value}, \code{y = node (reordered by Value)}. No \code{color} aesthetic is needed, but the legend is retained (though redundant).
    \item Assign class “\code{silent_gg}” to suppress warnings when printing.
    \item Return a list with \code{table = cents_single} and \code{plot = Figura}.
  \end{itemize}
}
}

\value{
A named list with two elements:
\itemize{
\item \code{table}: A \code{data.frame} containing
  \itemize{
    \item \code{full_name}: The original node name (character).
    \item \code{Abrev}: Three‐letter abbreviation of \code{full_name} (character).
    \item \code{measure0}: Z‐scored values of the selected standard centrality metric.
    \item \code{measure1}: (If \code{measure1} is not \code{NULL}) Z‐scored values of the selected bridge centrality metric.
  }
\item \code{plot}: A \code{ggplot2} object that displays nodes on the y‐axis (abbreviations or full names, depending on \code{use_abbrev}) and corresponding z‐scores on the x‐axis. Points and connecting lines distinguish each centrality measure. Legend and color schemes follow \code{color_palette} or \code{legend_labels}.
}
}

\examples{
\dontrun{
  # Example setup (requires qgraph and igraph)
  library(qgraph)
  library(igraph)

  # Generate a ring graph and its adjacency matrix
  set.seed(42)
  ig_obj <- igraph::make_ring(12)
  adj_mat <- igraph::as_adjacency_matrix(ig_obj, sparse = FALSE)
  network <- qgraph::qgraph(adj_mat)

  # Define community vector for 12 nodes (e.g., two communities of 6 each)
  groups_vec <- rep(1:2, each = 6)

  # 1) Compute and plot both Expected Influence and Bridge Expected Influence (1-step)
  result_both <- centrality_plots2(
    qgraph_obj    = ig_obj,
    network       = network,
    groups        = groups_vec,
    measure0      = "ExpectedInfluence",
    measure1      = "Bridge Expected Influence (1-step)",
    color_palette = c("#FF0000", "#00A08A"),
    labels        = NULL,
    legend_labels = NULL,
    use_abbrev    = FALSE
  )
  # View combined table and plot
  print(result_both$table)
  print(result_both$plot)

  # 2) Compute and plot only Expected Influence using abbreviations
  result_single <- centrality_plots2(
    qgraph_obj    = ig_obj,
    network       = network,
    measure0      = "ExpectedInfluence",
    measure1      = NULL,
    color_palette = c("#1B9E77"),
    use_abbrev    = TRUE
  )
  # View single‐metric table and plot
  print(result_single$table)
  print(result_single$plot)
}
}

\author{
Dr. José Ventura-León
}
