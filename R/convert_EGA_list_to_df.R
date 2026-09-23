#' Convert EGA List to Data Frame
#'
#' Converts a list of EGA results into a combined data frame.
#'
#' @param ega_list Named list of EGA result objects whose names follow the
#'   pattern \code{"<correlation>.<algorithm>"}, such as the output of
#'   \code{\link{run_EGA_combinations}}.
#'
#' @return A data frame with one row per EGA result: the combination name,
#'   the correlation method and algorithm parsed from it, and the metrics
#'   returned by \code{\link{convert_EGA_to_df}}.
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
#' combos <- run_EGA_combinations(items, corr = c("pearson", "spearman"),
#'                                algorithm = c("louvain", "walktrap"))
#' convert_EGA_list_to_df(combos)
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
