#' Iteratively refine items by bootstrap stability
#'
#' @param data A data frame of items to analyze.
#' @param threshold Stability threshold (default: 0.80).
#' @param corr Correlation method (default: "spearman").
#' @param model Network model (default: "glasso").
#' @param algorithm Community detection algorithm (default: "louvain").
#' @param iter Number of bootstrap iterations (default: 1000).
#' @param seed_start Starting seed value (default: 2025).
#' @param type Bootstrap type (default: "resampling").
#' @param ncores Number of cores for parallel processing (default: 11).
#' @param max_iter Maximum number of refinement iterations (default: 10).
#' @param plot.itemStability Logical; plot item stability (default: FALSE).
#'
#' @return A list containing the final bootstrap result, stability data, removed items, plots, and dimension consistency tables.
#' @export
#' @importFrom EGAnet bootEGA
#' @importFrom ggplot2 ggtitle theme element_text
#' @importFrom patchwork plot_layout
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr %>%
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
    max_iter     = 10,
    plot.itemStability = FALSE
) {

  current_data     <- data
  removed_items    <- character()
  plots            <- list()
  dims_consistency <- list()
  item_dimensions  <- list()

  for (i in seq_len(max_iter)) {
    seed <- seed_start + i - 1
    message(sprintf("Iteraci\u00f3n %d (seed = %d): %d \u00edtems en an\u00e1lisis ...",
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
      ncores    = ncores,
      plot.itemStability = plot.itemStability
    )

    # --- 2) Forzar comunidades numericas (solucion a cambio en EGAnet >= 2.0.5) ---
    boot.emat$EGA$wc <- as.numeric(as.factor(boot.emat$EGA$wc))

    # --- 3) Grafico EGA original ---
    ega_plot <- suppressWarnings(plot(boot.emat$EGA)) +
      ggtitle("Original Sample | EGA") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )

    # --- 4) Grafico de estabilidad ---
    stab_plot <- boot.emat$stability$item.stability$plot +
      ggtitle("Stability | Model refining") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )

    # --- 5) Combinar ---
    combined_plot <- ega_plot + stab_plot + patchwork::plot_layout(widths = c(1.2, 1))
    plots[[i]] <- combined_plot

    # --- 6) Consistencia estructural por dimension ---
    df_dim <- data.frame(
      Dimension   = names(boot.emat$stability$dimension.stability$structural.consistency),
      Consistency = as.numeric(boot.emat$stability$dimension.stability$structural.consistency),
      stringsAsFactors = FALSE
    )
    dims_consistency[[i]] <- df_dim

    # --- 7) Matriz all.dimensions por item ---
    df_item_dim <- boot.emat$stability$item.stability$item.stability$all.dimensions %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Item")
    item_dimensions[[i]] <- df_item_dim

    # --- 8) Evaluar estabilidad de items ---
    vec <- boot.emat$stability$item.stability$item.stability$empirical.dimensions
    df_emp <- data.frame(
      item      = names(vec),
      stability = as.numeric(vec),
      stringsAsFactors = FALSE
    )
    unstable <- df_emp$item[df_emp$stability < threshold]

    if (length(unstable) == 0) {
      message("[OK] Todos los \u00edtems alcanzaron estabilidad >= ", threshold,
              " tras ", i, " iteraci\u00f3n(es).")

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

    # --- 9) Eliminar items inestables ---
    message(sprintf("[WARNING] Eliminando %d \u00edtem(s): %s",
                    length(unstable), paste(unstable, collapse = ", ")))
    removed_items <- c(removed_items, unstable)
    current_data   <- current_data[, !colnames(current_data) %in% unstable, drop = FALSE]
  }

  stop("[ERROR] No se estabilizaron todos los \u00edtems tras ", max_iter,
       " iteraciones. \u00cdtems removidos: ", paste(removed_items, collapse = ", "))
}
