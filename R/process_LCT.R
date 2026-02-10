#' Process LCT (Loadings Comparison Test) results
#'
#' @param ... One or more LCT result objects, passed as named arguments.
#'
#' @return A tibble summarizing the LCT results.
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
process_LCT <- function(...) {
  # Capturamos tanto los valores como los nombres de los argumentos
  dot_vals  <- list(...)
  dot_exprs <- match.call(expand.dots = FALSE)$...
  dot_names <- sapply(dot_exprs, deparse)

  # Asignamos nombres a la lista
  lct_list <- setNames(dot_vals, dot_names)

  # Funcion interna para procesar un LCT
  .process_one <- function(lct, id) {
    tibble(
      id           = id,
      empirical    = lct$empirical,
      bootstrap    = lct$bootstrap,
      prop_Factor  = as.numeric(lct$proportion["Factor"]),
      prop_Network = as.numeric(lct$proportion["Network"])
    )
  }

  # Iteramos y combinamos en un data.frame
  bind_rows(
    lapply(names(lct_list), function(nm) {
      .process_one(lct_list[[nm]], nm)
    })
  )
}
