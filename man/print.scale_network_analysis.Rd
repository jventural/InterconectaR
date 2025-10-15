\name{print.scale_network_analysis}
\alias{print.scale_network_analysis}
\title{Print Method for \code{scale_network_analysis} Objects}
\description{
The \code{print.scale_network_analysis} method provides a concise, human-readable summary of objects returned by \code{compute_netScores}.
It displays a dataset overview, previews of network-based scores and dimension sum scores, the dimension--item mapping, and a truncated table of item loadings, facilitating quick inspection of results.
}
\usage{
\method{print}{scale_network_analysis}(x, ...)
}
\arguments{
\item{\code{x}}{An object of class \code{scale_network_analysis} as returned by \code{compute_netScores}.}
\item{\code{...}}{Further arguments passed to or from other methods (currently unused).}
}
\details{
The printed output includes the following sections:
\enumerate{
\item \strong{Dataset summary} (\code{x$data_summary}): rows, total columns, number of original variables, added network scores, and dimension sum scores.
\item \strong{Network Scores (first 6 rows)}: a preview of standardized network-based dimension scores (\code{x$net_scores}).
\item \strong{Dimension Sums (first 6 rows)}: a preview of raw item-sum scores by dimension (\code{x$dimension_sums}).
\item \strong{Dimension Summary}: a scale-by-dimension listing with the items retained per dimension (\code{x$dimension_summary}).
\item \strong{Item Loadings (first 10 rows)}: truncated table of item loadings sorted by absolute loading (\code{x$item_loadings}).
}
A final hint reminds users that the full merged dataset—including original variables, network scores, and sum scores—is available in \code{x$data_complete}.
}
\value{
Invisibly returns \code{x} (the input \code{scale_network_analysis} object), after printing a formatted summary to the console.
}
\examples{
\dontrun{
# Fit and compute network-based scores
res <- compute_netScores(
  scales = c("EDS", "BSFL", "WHO"),
  data = df_completo,
  min_loading = 0.25
)

# Print formatted summary (equivalent to calling print(res))
print(res)

# Access the full dataset with appended scores
head(res$data_complete)
}
}
\seealso{
\code{\link{compute_netScores}}
}
\author{
Dr. José Ventura-León
}
