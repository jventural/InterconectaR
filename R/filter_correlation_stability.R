#' Filter Correlation Stability
#'
#' Extracts and filters correlation stability indices from bootstrap results.
#'
#' @param caseDroppingBoot Case-dropping bootstrap results from
#'   `bootnet::bootnet(..., type = "case")`, computed with the statistics
#'   `"strength"`, `"expectedInfluence"`, `"bridgeStrength"` and
#'   `"bridgeExpectedInfluence"` (or a subset of them).
#'
#' @return A data frame with the columns `rowname` (the centrality index) and
#'   `Index` (its correlation stability coefficient, CS, rounded to two
#'   decimals).
#' @export
#' @examples
#' \donttest{
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
#' case_boot <- bootnet::bootnet(net, nBoots = 20, type = "case",
#'                               statistics = c("strength", "expectedInfluence"),
#'                               nCores = 1, verbose = FALSE)
#' filter_correlation_stability(case_boot)
#' }
#' @importFrom dplyr %>% filter
#' @importFrom tibble rownames_to_column
#' @importFrom bootnet corStability
filter_correlation_stability <- function(caseDroppingBoot) {
  # Calcular la estabilidad de las correlaciones
  CorStability <- bootnet::corStability(caseDroppingBoot)

  # Convertir en data frame, redondear y filtrar
  result <- data.frame(Index = round(CorStability, 2)) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(rowname %in% c("bridgeExpectedInfluence",
                          "bridgeStrength",
                          "expectedInfluence",
                          "strength"))

  return(result)
}
