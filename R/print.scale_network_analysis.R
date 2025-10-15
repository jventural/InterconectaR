print.scale_network_analysis <- function(x, ...) {
  cat(x$data_summary)

  cat("\n=== Network Scores (primeras 6 filas) ===\n")
  print(head(x$net_scores))

  cat("\n=== Dimension Sums (primeras 6 filas) ===\n")
  print(head(x$dimension_sums))

  cat("\n=== Dimension Summary ===")
  cat(x$dimension_summary)

  cat("\n\n=== Item Loadings (primeras 10 filas) ===\n")
  print(head(x$item_loadings, 10))

  cat("\n\nPara acceder al dataset completo con scores use: resultado$data_complete\n")

  invisible(x)
}
