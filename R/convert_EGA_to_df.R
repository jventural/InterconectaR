#' Convert EGA Result to Data Frame
#'
#' Extracts network metrics from a single EGA result object.
#'
#' @param ega_result A single EGA result object, as returned by
#'   \code{EGAnet::EGA()}.
#'
#' @return A one-row data frame with the network model, correlation method,
#'   community detection algorithm, lambda, number of nodes and edges,
#'   density, descriptive statistics of the non-zero edge weights, number of
#'   communities and TEFI.
#' @export
#' @examples
#' set.seed(123)
#' n <- 300
#' f1 <- rnorm(n)
#' f2 <- 0.3 * f1 + rnorm(n)
#' items <- data.frame(
#'   sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'   sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#' )
#' names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
#'
#' ega <- EGAnet::EGA(items, plot.EGA = FALSE)
#' convert_EGA_to_df(ega)
convert_EGA_to_df <- function(ega_result) {
  network_matrix <- ega_result$network
  methods <- attr(network_matrix, "methods")
  algorithm <- ega_result$algorithm
  if (is.null(algorithm)) algorithm <- attr(ega_result$wc, "methods")$algorithm

  metrics <- data.frame(
    Model = if (!is.null(methods$model)) toupper(methods$model) else NA,
    Correlation = if (!is.null(methods$corr)) methods$corr else NA,
    Algorithm = if (!is.null(algorithm)) algorithm else NA,
    Lambda = if (!is.null(methods$lambda)) formatC(methods$lambda, format = "f", digits = 3) else NA,
    Nodes = if (!is.null(nrow(network_matrix))) nrow(network_matrix) else NA,
    Edges = if (!is.null(sum(network_matrix != 0, na.rm = TRUE))) sum(network_matrix != 0, na.rm = TRUE)/2 else NA,
    Density = if (!is.null(mean(network_matrix != 0, na.rm = TRUE))) formatC(mean(network_matrix != 0, na.rm = TRUE), format = "f", digits = 3) else NA,
    Mean_Weight = if (!is.null(mean(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(mean(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    SD_Weight = if (!is.null(sd(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(sd(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    Min_Weight = if (!is.null(min(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(min(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    Max_Weight = if (!is.null(max(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(max(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    Communities = if (!is.null(ega_result$n.dim)) ega_result$n.dim else NA,
    TEFI = if (!is.null(ega_result$TEFI)) formatC(ega_result$TEFI, format = "f", digits = 3) else NA,
    stringsAsFactors = FALSE
  )

  return(metrics)
}
