#' Network Density Report
#'
#' Calculates and reports network density statistics.
#'
#' @param edge.matrix Network edge matrix (e.g., from qgraph).
#'
#' @return A character string reporting how many edges differ from zero out
#'   of the possible edges, and the resulting density as a percentage.
#' @export
#' @importFrom qgraph getWmat
#' @examples
#' w <- matrix(c(0, 0.3, 0,
#'               0.3, 0, 0.2,
#'               0, 0.2, 0), nrow = 3,
#'             dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
#' Density_report(w)
Density_report <- function(edge.matrix){
  n <- nrow(edge.matrix)
  Total_Density <- n*(n-1)/2
  Conexiones_Diff_cero <- sum(getWmat(edge.matrix) != 0)/2
  Densidad <- Conexiones_Diff_cero/Total_Density*100
  resultado <- paste(Conexiones_Diff_cero, " of ", Total_Density, " edges were distinct from zero (", format(Densidad, digits = 2, nsmall = 2), "% of density).", sep = "")
  return(resultado)
}
