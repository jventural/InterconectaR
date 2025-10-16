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
  require(EGAnet)
  require(tidyverse)
  require(patchwork)

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

    # --- 2) Forzar comunidades numéricas (solución a cambio en EGAnet >= 2.0.5) ---
    boot.emat$EGA$wc <- as.numeric(as.factor(boot.emat$EGA$wc))

    # --- 3) Gráfico EGA original ---
    ega_plot <- suppressWarnings(plot(boot.emat$EGA)) +
      ggtitle("Original Sample | EGA") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )

    # --- 4) Gráfico de estabilidad ---
    stab_plot <- boot.emat$stability$item.stability$plot +
      ggtitle("Stability | Model refining") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )

    # --- 5) Combinar ---
    combined_plot <- ega_plot + stab_plot + patchwork::plot_layout(widths = c(1.2, 1))
    plots[[i]] <- combined_plot

    # --- 6) Consistencia estructural por dimensión ---
    df_dim <- data.frame(
      Dimension   = names(boot.emat$stability$dimension.stability$structural.consistency),
      Consistency = as.numeric(boot.emat$stability$dimension.stability$structural.consistency),
      stringsAsFactors = FALSE
    )
    dims_consistency[[i]] <- df_dim

    # --- 7) Matriz all.dimensions por ítem ---
    df_item_dim <- boot.emat$stability$item.stability$item.stability$all.dimensions %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Item")
    item_dimensions[[i]] <- df_item_dim

    # --- 8) Evaluar estabilidad de ítems ---
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

      dims_renamed <- Map(function(df, iter) {
        colnames(df)[2] <- paste0("Consistency_iter", iter)
        df
      }, dims_consistency, seq_along(dims_consistency))

      dims_consistency_wide <- Reduce(
        function(x, y) merge(x, y, by = "Dimension", all = TRUE),
        dims_renamed
      )

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
        combined_plot           = combined_plot,
        n_iterations            = i
      ))
    }

    # --- 9) Eliminar ítems inestables ---
    message(sprintf("⚠️ Eliminando %d ítem(s): %s",
                    length(unstable), paste(unstable, collapse = ", ")))
    removed_items <- c(removed_items, unstable)
    current_data   <- current_data[, !colnames(current_data) %in% unstable, drop = FALSE]
  }

  stop("❌ No se estabilizaron todos los ítems tras ", max_iter,
       " iteraciones. Ítems removidos: ", paste(removed_items, collapse = ", "))
}
