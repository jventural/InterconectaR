#' Centrality-SD Correlation
#'
#' Calculates correlation between centrality indices and standard deviations.
#'
#' @param Data Data frame with the original data.
#' @param Centralitys Centrality results object with a `table` element.
#'
#' @export
#' @importFrom dplyr %>% inner_join rename
#' @importFrom tibble rownames_to_column
#' @importFrom correlation correlation
calculate_centrality_sd_correlation <- function(Data, Centralitys) {
  # Calcular el SD y convertirlo en un data frame
  SD <- apply(Data, 2, sd) %>%
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
