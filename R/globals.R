#' @name globals
#' @title Global variables and imports for InterconectaR
#'
#' @importFrom grDevices as.raster colorRamp colorRampPalette dev.off heat.colors jpeg pdf png rainbow rgb terrain.colors
#' @importFrom graphics par
#' @importFrom stats cor cov2cor end median na.omit prcomp predict promax sd setNames start
#' @importFrom utils head modifyList tail
#' @importFrom lavaan inspect
#' @keywords internal
NULL

utils::globalVariables(c(
  "Abrev", "Algorithm", "Centrality", "Correlation_Method", "FDR", "Group",
  "Measure", "Metric", "Model_Combination", "R2", "Sample_Size", "Symptoms",
  "TEFI", "V1", "V2", "Value",
  "abs_cor", "abs_w", "avg_TEFI", "avg_bias", "avg_sensitivity",
  "bias",
  "difference", "display_name", "domain", "dx", "dy",
  "end",
  "full_name",
  "getmatrix", "graph", "group",
  "intensity",
  "lab", "label", "len", "lx", "ly",
  "mean_value", "measure", "metric",
  "node", "node_name", "nx", "ny",
  "original_name",
  "precision",
  "r", "raw_bridge", "rowname",
  "s", "sd", "sensitivity", "specificity", "start",
  "tipo", "tx", "ty", "type",
  "ux", "uy",
  "value", "variable",
  "w",
  "x", "x0", "xend", "xi", "xj",
  "y", "y0", "y_label", "yend", "yi", "yj",
  "zscore"
))
