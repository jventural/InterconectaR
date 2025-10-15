\name{compute_netScores}
\alias{compute_netScores}
\title{Compute Network-Based Factor Scores for Multiple Psychological Scales}
\description{
The \code{compute_netScores} function automates the extraction of latent dimensions from multiple psychological scales using Exploratory Factor Analysis (EFA) combined with network centrality weighting.
It estimates dimension-level scores based on the relative importance (centrality) of each item in a partial correlation network (\pkg{qgraph}), producing both network-based scores and traditional item-sum scores for each identified dimension.
}
\usage{
compute_netScores(
  scales = c("EDS", "BSFL", "WHO"),
  data = df_completo,
  min_loading = 0.20,
  rename_dims = FALSE,
  custom_names = NULL
)
}
\arguments{
\item{\code{scales}}{Character vector. Scale prefixes to analyze (e.g., \code{c("EDS", "BSFL", "WHO")}). The function automatically detects all items in \code{data} whose names begin with these prefixes.}
\item{\code{data}}{A \code{data.frame} or \code{tibble} containing item-level responses. Columns must be named following scale prefixes (e.g., \code{EDS1}, \code{EDS2}, ...).}
\item{\code{min_loading}}{Numeric. Minimum absolute factor loading for an item to be retained within a dimension. Default is \code{0.20}.}
\item{\code{rename_dims}}{Logical. If \code{TRUE}, allows interactive or predefined renaming of extracted dimensions. Default is \code{FALSE}.}
\item{\code{custom_names}}{Optional list. Custom names to assign to each dimension (e.g., \code{list("EDS_Dim1" = "Emotional_Dysregulation_score")}). Automatically adds suffix \code{_score} if not present.}
}
\details{
\enumerate{
\item Automatically detects items for each specified scale based on name prefixes.
\item Performs an Exploratory Factor Analysis (\code{fa()}) using maximum likelihood estimation (\code{fm = "ml"}) and oblimin rotation.
\item Retains items with absolute loadings greater than or equal to \code{min_loading} within each factor.
\item Constructs a partial correlation network for items in each dimension using \pkg{qgraph} with graphical LASSO regularization.
\item Computes item centrality (\code{InDegree}) within each network, and derives participant-level scores weighted by these centralities (network scores).
\item Standardizes network scores across participants (\code{scale()}), and calculates corresponding item-sum scores for comparison.
\item Optionally renames dimensions interactively or from user-supplied names.
\item Returns a combined dataset with original variables, network-based scores, and dimension sum scores.
}
}
\value{
A list of class \code{"scale_network_analysis"} containing the following elements:
\itemize{
  \item{\code{data_complete}}{Tibble combining original data, network scores, and sum scores.}
  \item{\code{net_scores}}{Tibble with standardized network-based dimension scores.}
  \item{\code{dimension_sums}}{Tibble with raw sum scores for each dimension.}
  \item{\code{dimension_summary}}{Character string summarizing scales, dimensions, and items included.}
  \item{\code{item_loadings}}{Tibble of item loadings per dimension (sorted by absolute loading).}
  \item{\code{dimensions_list}}{Nested list mapping each scale’s dimensions and their corresponding items.}
  \item{\code{dimension_names}}{List of final dimension names (custom or default).}
  \item{\code{scale_columns}}{List of detected item columns per scale.}
  \item{\code{other_columns}}{Character vector of non-scale variables in the dataset.}
  \item{\code{data_summary}}{Character string summarizing dataset composition and counts.}
}
}
\examples{
\dontrun{
# Example with three psychological scales
res <- compute_netScores(
  scales = c("EDS", "BSFL", "WHO"),
  data = df_completo,
  min_loading = 0.25
)

# View summary of identified dimensions
cat(res$dimension_summary)

# Access standardized network scores
head(res$net_scores)

# Merge complete dataset with scores
df_with_scores <- res$data_complete
}
}
\author{
Dr. José Ventura-León
}
