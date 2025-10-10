\name{net_reduce2}
\alias{net_reduce2}
\title{Reduce Redundant Item Pairs in Psychometric Networks}

\description{
This function performs dimensionality reduction or variable selection on pairs of redundant items identified in a psychometric dataset. It serves as an improved and flexible version of \code{net_reduce}, allowing reduction through Principal Component Analysis (PCA) or by retaining the best-performing variable according to the \code{goldbricker} output.
}

\usage{
net_reduce2(data, badpairs, method = c("PCA", "best_goldbricker"), keep_unpaired = TRUE)
}

\arguments{
  \item{data}{A data frame (not a tibble) containing the original item-level data used to compute redundancy diagnostics.}
  \item{badpairs}{An object containing pairs of redundant items. This can be:
  \itemize{
    \item An object of class \code{"goldbricker"} (from the \code{networktools} package).
    \item A list or matrix of paired variable names.
  }}
  \item{method}{Character string specifying the reduction method. Options are:
  \itemize{
    \item \code{"PCA"}: combines each redundant pair into a single principal component (first component only).
    \item \code{"best_goldbricker"}: retains the item with the highest mean proportion in the \code{goldbricker} matrix.
  }}
  \item{keep_unpaired}{Logical. If \code{TRUE}, variables not included in any redundant pair are preserved in the final dataset. Defaults to \code{TRUE}.}
}

\details{
The function automates the reduction of redundant item pairs detected by methods such as \code{goldbricker()}. Internally, it performs the following steps:
\itemize{
  \item Converts the input to a standard \code{data.frame}.
  \item Extracts redundant pairs from the \code{badpairs} object.
  \item Removes duplicated pairs to avoid multiple reductions of the same variables.
  \item Applies the selected reduction method:
    \itemize{
      \item For \code{PCA}, both items are replaced by their first principal component (rank = 1), with optional median imputation for missing values.
      \item For \code{best_goldbricker}, the item with the higher mean redundancy proportion is retained.
    }
  \item Reconstructs a reduced dataset containing new or retained variables, depending on \code{keep_unpaired}.
}

In addition to the reduced dataset, the function generates detailed metadata about which items were removed, retained, or replaced.
}

\value{
A list containing the following elements:
\itemize{
  \item \code{data}: The reduced dataset with combined or retained variables.
  \item \code{removed_items}: A character vector of items eliminated during reduction.
  \item \code{added_variables}: Names of new variables created (e.g., PCA composites).
  \item \code{kept_from_original}: Variables retained without modification.
  \item \code{pair_summary}: A data frame summarizing the action taken for each redundant pair, including the method used, variables kept, and those removed.
}
}

\examples{
# Example with a goldbricker object
# (Assuming 'mydata' is a dataframe and 'gb' is a goldbricker object)
result <- net_reduce2(mydata, badpairs = gb, method = "best_goldbricker")

# Example using PCA reduction
result_pca <- net_reduce2(mydata, badpairs = gb, method = "PCA")

# Inspect reduced data
head(result_pca$data)

# Examine reduction summary
print(result_pca$pair_summary)
}

\author{
Dr. José Ventura-León
}

\seealso{
\code{\link{goldbricker}}, \code{\link{net_reduce}}
}

\references{
Jones, P. J., & colleagues (2021). *Networktools: Tools for identifying important nodes in networks.* R package version 1.4.2.
}
