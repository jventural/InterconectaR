#' Convert EGA List to Data Frame
#'
#' Converts a list of EGA results into a combined data frame.
#'
#' @param ega_list Named list of EGA result objects.
#'
#' @export
#' @importFrom purrr map_dfr
#' @importFrom tidyr separate
#' @importFrom dplyr %>%
convert_EGA_list_to_df <- function(ega_list) {
  # Filtrar elementos nulos y aplicar conversion
  purrr::map_dfr(
    .x = ega_list[!sapply(ega_list, is.null)],
    .f = convert_EGA_to_df,
    .id = "Model_Combination"
  ) %>%
    tidyr::separate(
      col = Model_Combination,
      into = c("Correlation", "Algorithm"),
      sep = "\\.",
      remove = FALSE
    )
}
