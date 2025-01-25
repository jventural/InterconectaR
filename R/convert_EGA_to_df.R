convert_EGA_to_df <- function(ega_result) {
  network_matrix <- ega_result$network
  methods <- attr(network_matrix, "methods")

  metrics <- data.frame(
    Model = if (!is.null(methods$model)) toupper(methods$model) else NA,
    Correlation = if (!is.null(methods$corr)) methods$corr else NA,
    Algorithm = if (!is.null(ega_result$algorithm)) ega_result$algorithm else NA,
    Lambda = if (!is.null(methods$lambda)) formatC(methods$lambda, format = "f", digits = 3) else NA,
    Nodes = if (!is.null(nrow(network_matrix))) nrow(network_matrix) else NA,
    Edges = if (!is.null(sum(network_matrix != 0, na.rm = TRUE))) sum(network_matrix != 0, na.rm = TRUE)/2 else NA,
    Density = if (!is.null(mean(network_matrix != 0, na.rm = TRUE))) formatC(mean(network_matrix != 0, na.rm = TRUE), format = "f", digits = 3) else NA,
    Mean_Weight = if (!is.null(mean(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(mean(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    SD_Weight = if (!is.null(sd(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(sd(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    Min_Weight = if (!is.null(min(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(min(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    Max_Weight = if (!is.null(max(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(max(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
    Communities = if (!is.null(ega_result$n.dim)) ega_result$n.dim else NA,
    TEFI = if (!is.null(ega_result$TEFI)) formatC(ega_result$TEFI, format = "f", digits = 3) else NA,
    stringsAsFactors = FALSE
  )

  return(metrics)
}
