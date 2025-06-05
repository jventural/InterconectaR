estimate_networks_by_group <- function(data,
                                        group_var,
                                        columns,
                                        default          = "ggmModSelect",
                                        stepwise         = TRUE,
                                        corMethod        = "spearman",
                                        abbreviate_vars  = FALSE,
                                        abbr_minlength   = 3) {
  # Instalar/cargar paquetes necesarios
  if (!requireNamespace("dplyr", quietly = TRUE))     install.packages("dplyr")
  if (!requireNamespace("bootnet", quietly = TRUE))   install.packages("bootnet")
  if (!requireNamespace("purrr", quietly = TRUE))     install.packages("purrr")

  library(dplyr)
  library(bootnet)
  library(purrr)

  # Obtener niveles únicos de group_var
  unique_groups <- unique(data[[group_var]])

  # Dividir el data.frame por grupo y quitar la columna de grupo
  list_of_data <- data %>%
    filter(!!sym(group_var) %in% unique_groups) %>%
    select(all_of(columns), !!sym(group_var)) %>%
    group_by(!!sym(group_var)) %>%
    group_split() %>%
    map(~ select(.x, -!!sym(group_var))) %>%
    setNames(unique_groups)

  # Función auxiliar para estimar y (si corresponde) abreviar nombres
  estimate_and_maybe_abbrev <- function(df_group) {
    if (abbreviate_vars) {
      orig_names <- colnames(df_group)
      new_names  <- abbreviate(orig_names, minlength = abbr_minlength)
      colnames(df_group) <- new_names
    }

    # Estimar la red con (posibles) nombres abreviados
    net <- estimateNetwork(df_group,
                           default   = default,
                           stepwise  = stepwise,
                           corMethod = corMethod)

    if (abbreviate_vars) {
      # Si existe net$data, aplicar mismas abreviaturas
      if (!is.null(net$data)) {
        colnames(net$data) <- colnames(df_group)
      }
      # Si net$graph tiene dimnames, reemplazarlas también
      if (!is.null(net$graph) && !is.null(dimnames(net$graph))) {
        dimnames(net$graph) <- list(colnames(df_group), colnames(df_group))
      }
    }

    net
  }

  # Aplicar la estimación (y abreviación condicional) a cada grupo
  list_of_networks <- map(list_of_data, estimate_and_maybe_abbrev)

  return(list_of_networks)
}
