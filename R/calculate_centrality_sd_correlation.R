#' Centrality-SD Correlation
#'
#' Calculates correlation between centrality indices and standard deviations.
#'
#' @param Data Data frame with the original data (one column per node).
#' @param Centralitys A list with a `table` element: a data frame with a
#'   `node` column and two centrality indices in its third and fourth
#'   columns.
#'
#' @return A `correlation` object (from the correlation package) with the
#'   correlation of each of the two centrality indices with the standard
#'   deviation of the nodes.
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
#' net <- bootnet::estimateNetwork(items, default = "EBICglasso")
#' ct <- qgraph::centralityTable(net)
#' ct <- ct[ct$measure %in% c("Strength", "ExpectedInfluence"),
#'          c("graph", "node", "measure", "value")]
#' tab <- tidyr::pivot_wider(ct, names_from = "measure", values_from = "value")
#' calculate_centrality_sd_correlation(items, list(table = tab))
#' @importFrom dplyr %>% inner_join rename
#' @importFrom tibble rownames_to_column
#' @importFrom correlation correlation
calculate_centrality_sd_correlation <- function(Data, Centralitys) {
  # Calcular el SD y convertirlo en un data frame
  SD <- apply(Data, 2, sd, na.rm = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column(var = "node") %>%
    rename(sd = ".")

  # Unir los datos de centralidad y SD
  combined_data <- inner_join(Centralitys$table, SD, by = "node")

  # Seleccionar las columnas 3, 4 y 5 dinamicamente
  column3 <- colnames(combined_data)[3]
  column4 <- colnames(combined_data)[4]
  column5 <- colnames(combined_data)[5]

  # Calcular la correlacion
  result <- combined_data %>%
    correlation::correlation(select = c(column3, column4), select2 = column5)

  return(result)
}
