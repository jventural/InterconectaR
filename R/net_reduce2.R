net_reduce2 <- function(data, badpairs, method = c("PCA", "best_goldbricker"),
                        keep_unpaired = TRUE) {
  method <- match.arg(method)

  # Asegurar data.frame (no tibble)
  if ("tbl" %in% class(data)) data <- as.data.frame(data)

  # --- Extraer pares del objeto badpairs (igual que net_reduce) ---
  if ("goldbricker" %in% class(badpairs)) {
    bp_full <- unlist(strsplit(names(badpairs$suggested_reductions), split = " & "))
  } else if (is.list(badpairs)) {
    bp_full <- unlist(badpairs)
  } else {
    bp_full <- as.vector(t(badpairs))
  }
  if (length(bp_full) == 0) {
    # No hay pares: devolver info mínima
    return(list(
      data = data,
      removed_items = character(0),
      added_variables = character(0),
      kept_from_original = colnames(data),
      pair_summary = data.frame()
    ))
  }
  bp_mat <- matrix(bp_full, ncol = 2, byrow = TRUE)

  # Manejo de duplicados (igual que net_reduce)
  bp_unable_vec <- matrix(duplicated(bp_full), ncol = 2, byrow = TRUE)[,1] |
    matrix(duplicated(bp_full), ncol = 2, byrow = TRUE)[,2]
  bp_unable <- bp_mat[bp_unable_vec, , drop = FALSE]
  if (all(is.na(bp_unable))) {
    bp <- bp_mat
    out_unable <- NULL
  } else if (is.vector(bp_unable)) {
    bp <- bp_mat[!bp_unable_vec, , drop = FALSE]
    out_unable <- paste(bp_unable[1], bp_unable[2], sep = " & ")
  } else {
    bp <- bp_mat[!bp_unable_vec, , drop = FALSE]
    out_unable <- paste(bp_unable[,1], bp_unable[,2], sep = " & ")
  }
  if (length(out_unable)) {
    warning("Pares no reducidos por duplicados en pares previos: ",
            paste(out_unable, collapse = ", "))
  }

  # --- Preparar columnas nuevas y resumen por par ---
  newcols <- matrix(numeric(), nrow = nrow(data), ncol = nrow(bp))
  bp_colnames <- character(nrow(bp))
  pair_summary <- data.frame(
    item_a = bp[,1],
    item_b = bp[,2],
    action = ifelse(method == "PCA", "PCA", "keep_one"),
    kept_column = NA_character_,
    new_variable = NA_character_,
    stringsAsFactors = FALSE
  )

  if (method == "PCA") {
    # Imputación mediana si hay NA (igual que net_reduce)
    if (TRUE %in% is.na.data.frame(data)) {
      warning("NAs en data: se imputan medianas por defecto para PCA.")
      data <- data.frame(lapply(data, function(x) {
        if (is.numeric(x)) ifelse(is.na(x), median(x, na.rm = TRUE), x) else x
      }))
    }
    for (i in seq_len(nrow(bp))) {
      comp <- prcomp(data[, bp[i, 1:2]], scale. = TRUE, rank. = 1)$x
      newcols[, i] <- comp[, 1]
      # Alinear con la dirección mayoritaria (igual a net_reduce)
      if (cor(newcols[, i], data[, bp[i, 1]]) < 0 & cor(newcols[, i], data[, bp[i, 2]]) < 0) {
        newcols[, i] <- -newcols[, i]
      }
      bp_colnames[i] <- paste("PCA", bp[i, 1], bp[i, 2], sep = ".")
      pair_summary$new_variable[i] <- bp_colnames[i]
      # En PCA no hay "kept" original; ambos se reemplazan por la nueva
    }
  } else if (method == "best_goldbricker") {
    if (!("goldbricker" %in% class(badpairs))) {
      stop("Para method='best_goldbricker', badpairs debe ser un objeto de clase 'goldbricker'.")
    }
    for (i in seq_len(nrow(bp))) {
      m1 <- mean(na.omit(badpairs$proportion_matrix[, bp[i, 1]]))
      m2 <- mean(na.omit(badpairs$proportion_matrix[, bp[i, 2]]))
      if (m1 >= m2) {
        newcols[, i] <- data[, bp[i, 1]]
        bp_colnames[i] <- bp[i, 1]
        pair_summary$kept_column[i] <- bp[i, 1]
        pair_summary$new_variable[i] <- bp[i, 1]
      } else {
        newcols[, i] <- data[, bp[i, 2]]
        bp_colnames[i] <- bp[i, 2]
        pair_summary$kept_column[i] <- bp[i, 2]
        pair_summary$new_variable[i] <- bp[i, 2]
      }
    }
  }
  colnames(newcols) <- bp_colnames

  # --- Construir data final ---
  # Ítems involucrados en pares
  paired_items <- unique(as.vector(bp))
  # Ítems no pareados en data original
  unpaired_items <- setdiff(colnames(data), paired_items)

  if (keep_unpaired) {
    # Igual que net_reduce: quita todos los de 'bp' y añade columnas nuevas
    data_stripped <- data[, !colnames(data) %in% as.vector(bp), drop = FALSE]
    newdata <- cbind(data_stripped, newcols)
  } else {
    # Solo columnas nuevas (PCA o kept), sin conservar los no pareados
    newdata <- as.data.frame(newcols, check.names = FALSE)
  }

  # Para el caso extremo de vector
  if (is.vector(newdata) && !is.null(colnames(data))) {
    colnames(newdata)[1] <- colnames(data)[!colnames(data) %in% as.vector(bp)]
  }

  # --- Metadatos útiles ---
  original_names <- colnames(data)
  reduced_names  <- colnames(newdata)

  removed_items      <- setdiff(original_names, reduced_names)
  added_variables    <- setdiff(reduced_names, original_names)
  kept_from_original <- intersect(original_names, reduced_names)

  # Enriquecer pair_summary con qué se eliminó por par (informativo)
  pair_summary$removed_by_pair <- NA_character_
  if (method == "best_goldbricker") {
    pair_summary$removed_by_pair <- ifelse(
      pair_summary$kept_column == pair_summary$item_a, pair_summary$item_b, pair_summary$item_a
    )
  } else if (method == "PCA") {
    # En PCA ambos originales se "reemplazan" por la nueva variable
    pair_summary$removed_by_pair <- paste(pair_summary$item_a, pair_summary$item_b, sep = " & ")
  }

  list(
    data = newdata,
    removed_items = removed_items,
    added_variables = added_variables,
    kept_from_original = kept_from_original,
    pair_summary = pair_summary
  )
}
