#' Filter Correlation Stability
#'
#' Extracts and filters correlation stability indices from bootstrap results.
#'
#' @param caseDroppingBoot Bootstrap results from bootnet case-dropping.
#'
#' @export
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
