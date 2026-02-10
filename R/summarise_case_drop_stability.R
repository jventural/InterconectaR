#' @title Summarise Case Drop Stability
#' @description Summarises case-dropping bootstrap stability results from bootnet.
#' @param caseDroppingBoot A bootnet case-dropping bootstrap object.
#' @param probs Numeric vector of length 2 for confidence interval quantiles.
#' @param correlation_use Character string for cor() use argument.
#' @param quiet Logical, suppress printing.
#' @return A data frame with stability summary statistics.
#' @export
#' @importFrom dplyr distinct select rename left_join group_by summarise mutate arrange n
#' @importFrom rlang .data
summarise_case_drop_stability <- function(caseDroppingBoot,
                                          probs = c(0.025, 0.975),
                                          correlation_use = "pairwise.complete.obs",
                                          quiet = TRUE) {
  dplyr <- asNamespace("dplyr")

  # Validaciones basicas
  if (!all(c("bootTable", "sampleTable") %in% names(caseDroppingBoot))) {
    stop("El objeto no parece provenir de bootnet: faltan 'bootTable' y/o 'sampleTable'.")
  }
  if (length(probs) != 2 || any(is.na(probs))) {
    stop("'probs' debe ser num\u00e9rico de longitud 2, p. ej., c(0.025, 0.975).")
  }
  probs <- sort(probs)

  boot <- caseDroppingBoot$bootTable
  samp <- caseDroppingBoot$sampleTable

  needed_sample <- c("type", "id", "value", "nPerson")
  needed_boot   <- c("name", "type", "id", "value", "nPerson")

  if (!all(needed_sample %in% names(samp))) {
    stop("A 'sampleTable' le faltan columnas: ",
         paste(setdiff(needed_sample, names(samp)), collapse = ", "))
  }
  if (!all(needed_boot %in% names(boot))) {
    stop("A 'bootTable' le faltan columnas: ",
         paste(setdiff(needed_boot, names(boot)), collapse = ", "))
  }

  # 1) Vector de referencia (muestra completa) por estadistico y nodo
  orig <- dplyr$distinct(
    dplyr$select(samp, "type", "id", "value")
  )
  orig <- dplyr$rename(orig, orig_value = "value")

  # Tamano original N0 (tomamos el maximo por seguridad)
  N0 <- suppressWarnings(max(samp$nPerson, na.rm = TRUE))

  # 2) Correlaciones boot vs. original por nivel de nPerson y estadistico
  plot_df <-
    boot |>
    dplyr$select("name", "type", "id", "value", "nPerson") |>
    dplyr$left_join(orig, by = c("type", "id")) |>
    dplyr$group_by(.data$type, .data$nPerson, .data$name) |>
    dplyr$summarise(
      cor_boot = suppressWarnings(
        stats::cor(.data$value, .data$orig_value, use = correlation_use)
      ),
      .groups = "drop"
    ) |>
    dplyr$group_by(.data$type, .data$nPerson) |>
    dplyr$summarise(
      mean_cor = mean(.data$cor_boot, na.rm = TRUE),
      lo       = stats::quantile(.data$cor_boot, probs[1], na.rm = TRUE),
      hi       = stats::quantile(.data$cor_boot, probs[2], na.rm = TRUE),
      n_boot   = dplyr$n(),
      .groups  = "drop"
    ) |>
    dplyr$mutate(p_sampled = .data$nPerson / N0) |>
    dplyr$arrange(.data$type, .data$nPerson)

  if (!quiet) print(utils::head(plot_df))
  return(plot_df)
}
