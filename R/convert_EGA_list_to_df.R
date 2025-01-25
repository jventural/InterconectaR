convert_EGA_list_to_df <- function(ega_list) {
  # Filtrar elementos nulos y aplicar conversión
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
