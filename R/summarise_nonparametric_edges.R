summarise_nonparametric_edges <- function(nonParametricBoot,
                                          stat_type = "edge",
                                          probs = c(0.025, 0.975),
                                          quiet = TRUE) {
  # Dependencias
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Se requiere el paquete 'dplyr'.")
  }
  dplyr <- asNamespace("dplyr")

  # Validaciones
  if (!all(c("bootTable", "sampleTable") %in% names(nonParametricBoot))) {
    stop("El objeto no parece provenir de bootnet: faltan 'bootTable' y/o 'sampleTable'.")
  }
  if (length(probs) != 2 || any(is.na(probs))) {
    stop("'probs' debe ser numérico de longitud 2, por ejemplo c(0.025, 0.975).")
  }
  probs <- sort(probs)

  boot <- nonParametricBoot$bootTable
  samp <- nonParametricBoot$sampleTable

  needed_cols <- c("type", "id", "value")
  if (!all(needed_cols %in% names(samp))) {
    stop("A 'sampleTable' le faltan columnas: ",
         paste(setdiff(needed_cols, names(samp)), collapse = ", "))
  }
  if (!all(needed_cols %in% names(boot))) {
    stop("A 'bootTable' le faltan columnas: ",
         paste(setdiff(needed_cols, names(boot)), collapse = ", "))
  }

  # 1) Muestra (puntos rojos)
  sample_edges <- samp |>
    dplyr$filter(.data$type == stat_type) |>
    dplyr$select("id", sample = "value")

  # 2) Resumen bootstrap (puntos negros y banda)
  boot_edges <- boot |>
    dplyr$filter(.data$type == stat_type) |>
    dplyr$group_by(.data$id) |>
    dplyr$summarise(
      boot_mean = mean(.data$value, na.rm = TRUE),
      ci_lo     = stats::quantile(.data$value, probs[1], na.rm = TRUE),
      ci_hi     = stats::quantile(.data$value, probs[2], na.rm = TRUE),
      n_boot    = dplyr$n(),
      .groups   = "drop"
    )

  # 3) Tabla final
  edge_plot_values <- sample_edges |>
    dplyr$left_join(boot_edges, by = "id") |>
    dplyr$mutate(
      ci_covers_zero = dplyr$if_else(
        !is.na(.data$ci_lo) & !is.na(.data$ci_hi),
        .data$ci_lo <= 0 & .data$ci_hi >= 0,
        NA
      )
    ) |>
    dplyr$arrange(dplyr$desc(abs(.data$boot_mean)))

  if (!quiet) print(utils::head(edge_plot_values))
  return(edge_plot_values)
}
