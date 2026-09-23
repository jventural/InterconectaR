#' MGM Errors by Group
#'
#' Fits Mixed Graphical Models and extracts errors for each group.
#'
#' @param data Data frame containing the data.
#' @param type Character vector of variable types.
#' @param level Integer vector of variable levels.
#' @param group Unquoted grouping variable name.
#' @param columns Character vector of column names to analyze.
#'
#' @return A named list with one tibble per group, holding the RMSE, R2, CC
#'   and nCC of each variable (in the order of `columns`).
#' @export
#' @examples
#' set.seed(123)
#' n <- 300
#' f1 <- rnorm(n)
#' f2 <- 0.3 * f1 + rnorm(n)
#' items <- data.frame(
#'   sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'   sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#' )
#' names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
#' items$sex <- rep(c("Female", "Male"), each = n / 2)
#'
#' mgm_errors_groups(items, type = rep("g", 8), level = rep(1, 8),
#'                   group = sex, columns = names(items)[1:8])
#' @importFrom mgm mgm
#' @importFrom dplyr %>% select all_of group_nest mutate pull
#' @importFrom purrr map map2
#' @importFrom tibble as_tibble
#' @importFrom tidyselect where
mgm_errors_groups <- function(data, type, level, group, columns) {
  # Seleccion de variables y agrupamiento
  mgm_group <- data %>%
    dplyr::select(dplyr::all_of(columns), {{ group }}) %>%
    dplyr::group_nest({{ group }}) %>%
    dplyr::mutate(
      data = purrr::map(data, ~ dplyr::select(., tidyselect::where(~ !all(is.na(.))))), # Eliminar columnas con todos los valores NA
      Estimacion = purrr::map(data, ~ mgm::mgm(as.matrix(.x),  # Convertir a matriz
                                   type = type,
                                   level = level,
                                   pbar = FALSE,
                                   signInfo = FALSE))
    )

  # Prediccion y almacenamiento
  predictive <- purrr::map2(mgm_group$Estimacion, mgm_group$data, ~
                       predict(object = .x,
                               data = .y,
                               errorCon = c("RMSE", "R2"),
                               errorCat = c("CC", "nCC", "CCmarg")))

  # Calculo de errores
  error <- purrr::map(predictive, ~ .x$errors %>%
                 tibble::as_tibble() %>%
                 dplyr::select(RMSE, R2, CC, nCC))

  # Nombrar los errores con los valores de 'group'
  names(error) <- mgm_group %>% dplyr::pull({{ group }})

  return(error)
}
