process_LCT <- function(...) {
  library(dplyr)
  # Capturamos tanto los valores como los nombres de los argumentos
  dot_vals  <- list(...)
  dot_exprs <- match.call(expand.dots = FALSE)$...
  dot_names <- sapply(dot_exprs, deparse)

  # Asignamos nombres a la lista
  lct_list <- setNames(dot_vals, dot_names)

  # Función interna para procesar un LCT
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
