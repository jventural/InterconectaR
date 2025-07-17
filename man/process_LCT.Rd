\name{process_LCT}
\alias{process_LCT}
\title{Combine Multiple LCT Results}
\description{
The \code{process_LCT} function accepts any number of LCT result objects (each containing \code{empirical}, \code{bootstrap}, and \code{proportion} entries) and returns a consolidated \code{\link[dplyr]{tibble}} with one row per LCT object. Each row reports the empirical and bootstrap estimates plus the proportions for “Factor” and “Network.”
}
\usage{
process_LCT(...)
}
\arguments{
\item{\code{...}}{One or more LCT objects, each typically a list with components \code{empirical}, \code{bootstrap}, and \code{proportion} (a named vector with entries “Factor” and “Network”). The object names supplied to \code{...} become the \code{id} column in the output.}
}
\details{
\enumerate{
\item Captures both the values and the original argument names passed via \code{...}.
\item Assigns those names to a list \code{lct_list}.
\item Defines an internal function \code{.process_one} that takes a single LCT object and its \code{id}, and builds a row with:
\itemize{
\item \code{id}: the name of the input object,
\item \code{empirical}: the empirical network or factor count,
\item \code{bootstrap}: the bootstrap network or factor count,
\item \code{prop_Factor}: numeric proportion for the “Factor” dimension,
\item \code{prop_Network}: numeric proportion for the “Network” dimension.
}
\item Iterates over all elements of \code{lct_list}, applies \code{.process_one}, and row‑binds them into a single \code{tibble} via \code{\link[dplyr]{bind_rows}}.
}
}
\value{
A \code{tibble} (data frame) with columns:
\itemize{
\item \code{id}: character, the original object name,
\item \code{empirical}: numeric or integer, the empirical count,
\item \code{bootstrap}: numeric or integer, the bootstrap count,
\item \code{prop_Factor}: numeric, the proportion of Factor solutions,
\item \code{prop_Network}: numeric, the proportion of Network solutions.
}
}
\examples{
\dontrun{

LCT1 <- LCT(Data_items_Explo,
    seed = 2025,
    iter = 1000,
    model = "glasso",
    algorithm = "walktrap",
    corr = "spearman",
    uni.method = c("LE"))

LCT2 <-LCT(removed_items,
    seed = 2025,
    iter = 1000,
    model = "glasso",
    algorithm = "walktrap",
    corr = "spearman",
    uni.method = c("LE"))

df_results <- process_LCT_list(LCT1, LCT2)
}
}
\author{
Dr. José Ventura-León
}
