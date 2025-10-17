\name{refine_items_by_stability}
\alias{refine_items_by_stability}
\title{Iteratively Refine Item Set by Stability Using bootEGA}
\description{
The \code{refine_items_by_stability} function applies iterative bootstrapped Exploratory Graph Analysis (via \code{bootEGA}) to identify and remove unstable items from a dataset. In each iteration, it computes item stability and structural consistency for each dimension, removes items whose empirical stability falls below a user‐specified threshold, and repeats until all remaining items achieve the desired stability or a maximum number of iterations is reached. The function returns both the final \code{bootEGA} object and a comprehensive record of removed items, stability metrics, and diagnostic plots across iterations.
}
\usage{
refine_items_by_stability(data,
                          threshold = 0.80,
                          corr = "spearman",
                          model = "glasso",
                          algorithm = "louvain",
                          iter = 1000,
                          seed_start = 2025,
                          type = "resampling",
                          ncores = 11,
                          max_iter = 10,
                          plot.itemStability = FALSE)
}
\arguments{
\item{\code{data}}{A data frame or matrix of item responses (cases in rows, items in columns).}
\item{\code{threshold}}{Numeric between 0 and 1. Minimum empirical stability required for each item to be retained. Items below this threshold are removed at each iteration. Default is 0.80.}
\item{\code{corr}}{Character. Type of correlation to use in network estimation (passed to \code{bootEGA}), e.g., \code{"spearman"} or \code{"pearson"}.}
\item{\code{model}}{Character. Network estimation model for \code{bootEGA}, such as \code{"glasso"} or \code{"hub"}.}
\item{\code{algorithm}}{Character. Community detection algorithm for Exploratory Graph Analysis, e.g., \code{"louvain"}, \code{"walktrap"}.}
\item{\code{iter}}{Integer. Number of bootstrap iterations to perform in each \code{bootEGA} run.}
\item{\code{seed_start}}{Integer. Starting seed for reproducibility. Each iteration adds \code{i - 1} to this base seed.}
\item{\code{type}}{Character. Type of resampling in \code{bootEGA}, either \code{"resampling"} or \code{"EBIC"}.}
\item{\code{ncores}}{Integer. Number of CPU cores to use for parallel bootstrap computation.}
\item{\code{max_iter}}{Integer. Maximum number of refinement iterations before the function stops with an error if stability is not achieved.}
\item{\code{plot.itemStability}}{Should the plot be produced for item.replication. Defaults to FALSE}
}
\details{
On each iteration \code{i} (up to \code{max_iter}):
\enumerate{
\item A new seed is set (\code{seed_start + i - 1}) and \code{bootEGA} is executed on the current item set.
\item The item‐stability plot and dimension‐stability metrics are stored.
\item Empirical stability for each item is extracted. Items with stability below \code{threshold} are flagged as unstable.
\item If no unstable items remain, iteration stops and the function proceeds to assemble final outputs.
\item Otherwise, all unstable items are removed from \code{data} and the next iteration begins.
}
If \code{max_iter} is reached without achieving the target stability for all items, the function aborts with an error listing all removed items.
}
\value{
A list with the following components:
\itemize{
\item \code{final_boot}: The \code{bootEGA} result from the last iteration.
\item \code{stability_df}: A data frame of empirical stability values for each item in the final set.
\item \code{removed_items}: Character vector of all items removed across iterations.
\item \code{plots}: List of \code{\link{ggplot2}} objects showing item‐stability across iterations.
\item \code{dims_consistency}: List of data frames with structural consistency metrics per dimension for each iteration.
\item \code{item_dimensions}: List of data frames with per‐item dimension membership frequencies per iteration.
\item \code{dims_consistency_wide}: A single wide data frame combining all \code{dims_consistency} tables with iteration‐specific column names.
\item \code{item_dimensions_joined}: A merged data frame combining all \code{item_dimensions} with iteration‐specific prefixes.
}
}
\examples{
\dontrun{

# Simulate some item data
set.seed(123)
my_data <- as.data.frame(matrix(rnorm(200*15), nrow=200, ncol=15))
colnames(my_data) <- paste0("Item", 1:15)

# Refine items by stability, removing those < .75, using 500 bootstraps
result <- refine_items_by_stability(data = my_data,
                                    threshold = 0.75,
                                    iter = 500,
                                    seed_start = 2025,
                                    ncores = 4,
                                    max_iter = 5,
                                    plot.itemStability = FALSE)

# View removed items and final stability
print(result$removed_items)
print(result$stability_df)
}
}
\author{
Dr. José Ventura-León
}
