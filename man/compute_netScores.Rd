\name{compute_netScores}
\alias{compute_netScores}
\title{Compute Network-Based Scores with Item Stability Refinement and Exploratory Graph Analysis (EGA)}
\description{
The \code{compute_netScores} function automates a multistage pipeline to extract, refine, and score latent dimensions from multiple psychological scales.
It integrates item-level stability analysis (via bootstrapped Exploratory Graph Analysis, \pkg{EGAnet}) with network-based factor score computation (\code{net.scores}), producing both standardized network scores and traditional sum scores.
This approach ensures that only stable items contribute to the latent dimensions, enhancing replicability and interpretability across scales.
}
\usage{
compute_netScores(
  item_prefixes,
  data,
  rename_dims = TRUE,
  custom_names = NULL,
  add_sums = TRUE,
  # Stability analysis parameters
  stability_threshold = 0.70,
  stability_corr = "spearman",
  stability_model = "glasso",
  stability_algorithm = "louvain",
  stability_iter = 1000,
  stability_seed_start = 2025,
  stability_type = "resampling",
  stability_ncores = 11,
  stability_max_iter = 10,
  stability_plot = TRUE,
  # EGA parameters
  ega_plot = TRUE
)
}
\arguments{
\item{\code{item_prefixes}}{Character vector with prefixes identifying each psychological scale (e.g., \code{c("BP", "SC", "EC")}). The function automatically detects items in \code{data} whose names begin with these prefixes.}

\item{\code{data}}{A \code{data.frame} or \code{tibble} containing item-level responses. Column names must follow the prefix convention (e.g., \code{BP1}, \code{BP2}, \code{BP3}, ...).}

\item{\code{rename_dims}}{Logical. If \code{TRUE}, allows renaming dimensions interactively or via predefined names in \code{custom_names}. Default is \code{TRUE}.}

\item{\code{custom_names}}{Optional list of user-defined names for dimensions. Automatically appends the suffix \code{_score} if not present.}

\item{\code{add_sums}}{Logical. If \code{TRUE}, adds traditional item-sum scores per dimension in addition to network-based scores. Default is \code{TRUE}.}

\item{\code{stability_threshold}}{Numeric. Minimum proportion of stability (default = \code{0.70}) required for an item to be retained during iterative resampling.}

\item{\code{stability_corr}}{Correlation method for the stability analysis. Options include \code{"spearman"} (default), \code{"pearson"}, or \code{"auto"}.}

\item{\code{stability_model}}{Network estimation model used in bootstrapped EGA (default = \code{"glasso"}).}

\item{\code{stability_algorithm}}{Community detection algorithm used in EGA (default = \code{"louvain"}).}

\item{\code{stability_iter}}{Integer. Number of bootstrap iterations (default = \code{1000}).}

\item{\code{stability_seed_start}}{Integer. Starting seed value for reproducibility across iterations (default = \code{2025}).}

\item{\code{stability_type}}{Type of resampling for the bootstrapped EGA. Options: \code{"resampling"} (default) or \code{"parametric"}.}

\item{\code{stability_ncores}}{Integer. Number of CPU cores used for parallel processing during bootstrapping (default = \code{11}).}

\item{\code{stability_max_iter}}{Maximum number of refinement iterations to achieve item stability (default = \code{10}).}

\item{\code{stability_plot}}{Logical. If \code{TRUE}, displays item stability plots for each iteration. Default is \code{TRUE}.}

\item{\code{ega_plot}}{Logical. If \code{TRUE}, displays the EGA network plot for each refined scale. Default is \code{TRUE}.}
}
\details{
The function performs the following steps for each psychological scale:

\enumerate{
\item \strong{Item Extraction:} Automatically selects variables in \code{data} whose names start with the given prefix.
\item \strong{Item Stability Analysis:} Iteratively refines the item set using \code{refine_items_by_stability()}, which bootstraps EGA structures and removes items with dimension membership instability below \code{stability_threshold}.
\item \strong{Exploratory Graph Analysis (EGA):} Estimates dimensional structure using \pkg{EGAnet} on the refined dataset, identifying item communities (latent dimensions).
\item \strong{Network Score Computation:} Uses \code{net.scores()} to calculate participant-level standardized scores, weighting each item by its centrality within the network.
\item \strong{Sum Scores:} Optionally computes unweighted sum scores per dimension for comparison and validation.
\item \strong{Dimension Naming:} Allows user-defined or automatic naming of dimensions (e.g., \code{BP_Dim1} → \code{BP_NetScore_1}).
\item \strong{Output Combination:} Returns a unified dataset containing original variables, network-based scores, and optional sum scores, along with all intermediate results (EGA and stability objects).
}
}
\value{
A list of class \code{"ega_network_scores"} containing:
\itemize{
  \item{\code{data_complete}}{Tibble combining original data, network scores, and (optionally) sum scores.}
  \item{\code{net_scores}}{Tibble of standardized network-based dimension scores per participant.}
  \item{\code{dimension_sums}}{Tibble of dimension-wise sum scores (if \code{add_sums = TRUE}).}
  \item{\code{dimensions_list}}{Nested list mapping each scale’s dimensions and their corresponding items.}
  \item{\code{dimension_names}}{Named list of final dimension labels (user-defined or auto-generated).}
  \item{\code{ega_objects}}{List of \code{EGA} results per scale.}
  \item{\code{stability_results}}{List of \code{refine_items_by_stability} outputs per scale, including item removal logs and plots.}
  \item{\code{removed_items}}{List of items excluded during stability refinement for each scale.}
  \item{\code{scale_columns}}{Detected item columns per scale prefix.}
}
}
\examples{
\dontrun{
# Example with three scales and stability refinement
res <- compute_netScores(
  item_prefixes = c("BP", "SC", "EC"),
  data = df_completo,
  stability_threshold = 0.80,
  rename_dims = TRUE
)

# Inspect network-based scores
head(res$net_scores)

# Review dimensions and removed items
res$dimensions_list
res$removed_items

# Access the full dataset with appended scores
df_final <- res$data_complete
}
}
\author{
Dr. José Ventura-León
}
