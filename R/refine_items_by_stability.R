refine_items_by_stability <- function(
    data,
    threshold    = 0.80,
    corr         = "spearman",
    model        = "glasso",
    algorithm    = "louvain",
    iter         = 1000,
    seed_start   = 2025,
    type         = "resampling",
    ncores       = 11,
    max_iter     = 10
) {
  current_data     <- data
  removed_items    <- character()
  plots            <- list()
  dims_consistency <- list()
  item_dimensions  <- list()

  for (i in seq_len(max_iter)) {
    seed <- seed_start + i - 1
    message(sprintf("Iteración %d (seed = %d): %d ítems en análisis …",
                    i, seed, ncol(current_data)))

    # --- 1) Ejecutar bootEGA ---
    boot.emat <- bootEGA(
      data      = current_data,
      corr      = corr,
      model     = model,
      algorithm = algorithm,
      iter      = iter,
      seed      = seed,
      type      = type,
      ncores    = ncores
    )

    # --- 2) Guardar gráfico por iteración ---
    plots[[i]] <- boot.emat$stability$item.stability$plot

    # --- 3) Consistencia estructural por dimensión ---
    df_dim <- data.frame(
      Dimension   = names(boot.emat$stability$dimension.stability$structural.consistency),
      Consistency = as.numeric(boot.emat$stability$dimension.stability$structural.consistency),
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
    dims_consistency[[i]] <- df_dim

    # --- 4) Matriz all.dimensions por ítem ---
    df_item_dim <- boot.emat$stability$item.stability$item.stability$all.dimensions %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Item")
    item_dimensions[[i]] <- df_item_dim

    # --- 5) Comprobar estabilidad de ítems ---
    vec <- boot.emat$stability$item.stability$item.stability$empirical.dimensions
    df_emp <- data.frame(
      item      = names(vec),
      stability = as.numeric(vec),
      stringsAsFactors = FALSE
    )
    unstable <- df_emp$item[df_emp$stability < threshold]

    if (length(unstable) == 0) {
      message("✅ Todos los ítems alcanzaron estabilidad ≥ ", threshold,
              " tras ", i, " iteración(es).")

      # --- 6) CORRECCIÓN: Renombrar y combinar dims_consistency con merge ---
      dims_renamed <- Map(function(df, iter) {
        colnames(df)[2] <- paste0("Consistency_iter", iter)
        df
      }, dims_consistency, seq_along(dims_consistency))

      # Hacer merge por "Dimension" en lugar de cbind
      dims_consistency_wide <- Reduce(
        function(x, y) merge(x, y, by = "Dimension", all = TRUE),
        dims_renamed
      )

      # --- 7) Renombrar y unir item_dimensions ---
      item_renamed <- Map(function(df, iter) {
        nm <- colnames(df)
        nm[-1] <- paste0("iter", iter, "_", nm[-1])
        setNames(df, nm)
      }, item_dimensions, seq_along(item_dimensions))

      item_dimensions_joined <- Reduce(
        function(x, y) merge(x, y, by = "Item", all = TRUE),
        item_renamed
      )

      return(list(
        final_boot              = boot.emat,
        stability_df            = df_emp,
        removed_items           = removed_items,
        plots                   = plots,
        dims_consistency        = dims_consistency,
        item_dimensions         = item_dimensions,
        dims_consistency_wide   = dims_consistency_wide,
        item_dimensions_joined  = item_dimensions_joined,
        n_iterations            = i  # Útil para saber cuántas iteraciones se necesitaron
      ))
    }

    # --- 8) Eliminar ítems inestables y seguir iterando ---
    message(sprintf("⚠️ Eliminando %d ítem(s): %s",
                    length(unstable), paste(unstable, collapse = ", ")))
    removed_items <- c(removed_items, unstable)
    current_data   <- current_data[, !colnames(current_data) %in% unstable, drop = FALSE]
  }

  stop("❌ No se estabilizaron todos los ítems tras ", max_iter,
       " iteraciones. Ítems removidos: ", paste(removed_items, collapse = ", "))
}
