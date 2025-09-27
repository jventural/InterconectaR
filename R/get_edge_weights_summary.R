get_edge_weights_summary <- function(network,
                                     abs_weights  = T,  # usar |w|?
                                     round_digits = 2) {    # redondeo SOLO de la salida
  # --- Opciones internas (fijas) ---
  unique_edges     <- TRUE   # contar cada arista una vez (si es simétrica)
  include_diagonal <- FALSE  # excluir diagonal
  drop_zeros       <- TRUE   # excluir pesos == 0

  # 1) Obtener matriz de pesos
  W <- getWmat(network$graph)

  # 2) Selección de entradas
  if (unique_edges && isSymmetric(W)) {
    idx <- upper.tri(W, diag = include_diagonal)
    w <- W[idx]
  } else {
    w <- if (include_diagonal) as.vector(W) else W[row(W) != col(W)]
  }

  # 3) Opciones internas: quitar ceros y tomar absoluto
  if (drop_zeros) w <- w[w != 0]
  if (abs_weights) w <- abs(w)

  # Guard: si no hay datos válidos
  if (length(w) == 0 || all(is.na(w))) {
    out <- data.frame(N = 0, Mean = NA_real_, SD = NA_real_, Min = NA_real_, Max = NA_real_)
    if (!is.null(round_digits)) out[,-1] <- round(out[,-1], round_digits)
    return(out)
  }

  # 4) Descriptivos (sin redondear)
  out <- data.frame(
    N    = length(w),
    Mean = mean(w, na.rm = TRUE),
    SD   = sd(w,   na.rm = TRUE),
    Min  = suppressWarnings(min(w, na.rm = TRUE)),
    Max  = suppressWarnings(max(w, na.rm = TRUE))
  )

  # 5) Redondeo de presentación
  if (!is.null(round_digits)) out[,-1] <- round(out[,-1], round_digits)

  return(out)
}
