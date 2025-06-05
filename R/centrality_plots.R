centrality_plots <- function(qgraph_obj,
                              network,
                              groups = NULL,
                              measure0 = "ExpectedInfluence",
                              measure1 = NULL,
                              color_palette = c("#FF0000", "#00A08A"),
                              labels = NULL,
                              legend_labels = NULL,
                              use_abbrev = TRUE) {
  ################################################################################
  # 1) Instalación y carga de librerías
  ################################################################################
  if (!require("qgraph",      quietly = TRUE)) install.packages("qgraph",      dependencies = TRUE)
  if (!require("dplyr",       quietly = TRUE)) install.packages("dplyr",       dependencies = TRUE)
  if (!require("networktools",quietly = TRUE)) install.packages("networktools",dependencies = TRUE)
  if (!require("tibble",      quietly = TRUE)) install.packages("tibble",      dependencies = TRUE)
  if (!require("ggplot2",     quietly = TRUE)) install.packages("ggplot2",     dependencies = TRUE)
  if (!require("tidyr",       quietly = TRUE)) install.packages("tidyr",       dependencies = TRUE)
  if (!require("stringr",     quietly = TRUE)) install.packages("stringr",     dependencies = TRUE)
  if (!require("forcats",     quietly = TRUE)) install.packages("forcats",     dependencies = TRUE)

  library(qgraph)
  library(dplyr)
  library(networktools)
  library(tibble)
  library(ggplot2)
  library(tidyr)
  library(stringr)
  library(forcats)

  ################################################################################
  # 2) Extraer tabla de centralidad para measure0
  ################################################################################
  ctbl <- centralityTable(network)

  cents_expect <- ctbl %>%
    filter(measure == !!measure0) %>%
    select(full_name = node, value) %>%
    arrange(full_name) %>%
    rename(!!measure0 := value)

  # Etiquetas personalizadas (si las hay): full_name -> custom label
  if (!is.null(labels)) {
    cents_expect <- cents_expect %>%
      mutate(full_name = if_else(full_name %in% names(labels),
                                 labels[full_name],
                                 full_name))
  }

  # -------------------------------------------------
  # 2.1) Abreviar “full_name” siempre (primeras 3 letras de la primera palabra)
  # -------------------------------------------------
  cents_expect <- cents_expect %>%
    mutate(
      Abrev = full_name %>%
        str_split(" ") %>%        # separar en palabras
        map_chr(~ .x[[1]]) %>%    # seleccionar primera palabra
        substr(1, 3) %>%          # tomar primeros 3 caracteres
        str_to_title()            # Title Case
    )

  ################################################################################
  # 3) Si measure1 no es NULL: calcular puente
  ################################################################################
  if (!is.null(measure1)) {
    # 3.1) Ejecutar bridge()
    b_obj <- bridge(
      qgraph_obj,
      communities    = groups,
      useCommunities = "all",
      normalize      = FALSE
    )

    # 3.2) Convertir b_obj[[measure1]] en data.frame preservando nombres (abreviaturas)
    bridge_data <- tibble::enframe(
      b_obj[[measure1]],
      name  = "Abrev",    # Abrev coincide con la columna Abrev de cents_expect
      value = "raw_bridge"
    ) %>%
      mutate(!!measure1 := as.numeric(scale(raw_bridge))) %>%
      select(-raw_bridge) %>%
      arrange(Abrev)

    # 3.3) Unir bridge_data con cents_expect por “Abrev”
    cents2 <- inner_join(
      bridge_data,
      cents_expect,
      by = "Abrev"
    ) %>%
      select(full_name, Abrev, !!sym(measure0), !!sym(measure1))

    ################################################################################
    # 4A) Preparar datos en formato “largo” para ggplot (dos métricas)
    ################################################################################
    # Definir qué variable usaremos en el eje y:
    y_var <- if (use_abbrev) quo(Abrev) else quo(full_name)

    cents_long <- cents2 %>%
      pivot_longer(
        cols      = c(!!sym(measure0), !!sym(measure1)),
        names_to  = "Centrality",
        values_to = "Value"
      ) %>%
      rename(Measure = Centrality)

    ################################################################################
    # 5A) Definir paleta y etiquetas de leyenda
    ################################################################################
    if (is.null(legend_labels)) {
      pal         <- setNames(color_palette, c(measure1, measure0))
      legend_lbls <- setNames(c(measure1, measure0), c(measure1, measure0))
    } else {
      pal         <- setNames(color_palette, c(measure1, measure0))
      legend_lbls <- legend_labels
    }

    ################################################################################
    # 6A) Construir ggplot con dos líneas y suprimir warnings en PRINT
    ################################################################################
    Figura <-  ggplot(
      cents_long,
      aes(
        x     = Value,
        y     = fct_reorder(!!y_var, Value),
        color = Measure,
        group = Measure
      )
    ) +
      geom_point(size = 3) +
      geom_line(size = 0.5) +
      theme_minimal() +
      labs(
        x     = "z-score",
        y     = "Nodos",
        color = "Métrica"
      ) +
      scale_color_manual(
        values = pal,
        labels = legend_lbls
      ) +
      theme(
        axis.text.y     = element_text(size = 12),
        axis.text.x     = element_text(size = 12),
        legend.text     = element_text(size = 12),
        legend.title    = element_text(size = 12),
        axis.title.y    = element_text(size = 12),
        axis.title.x    = element_text(size = 12),
        legend.position = "bottom"
      )

    # Asignar clase personalizada para suprimir advertencias al imprimir
    class(Figura) <- c("silent_gg", class(Figura))
    assign("print.silent_gg",
           function(x, ...) suppressWarnings(NextMethod()),
           envir = .GlobalEnv)

    ################################################################################
    # 7A) Devolver tabla y gráfico (caso dual)
    ################################################################################
    return(
      list(
        table = cents2 %>% arrange(desc(!!sym(measure0))),
        plot  = Figura
      )
    )
  }

  ################################################################################
  # 4B) Caso measure1 = NULL: solo measure0
  ################################################################################
  cents_single <- cents_expect %>%
    select(full_name, Abrev, !!sym(measure0)) %>%
    arrange(desc(!!sym(measure0))) %>%
    rename(Value = !!sym(measure0))

  ################################################################################
  # 5B) Preparar paleta y leyenda para una sola métrica
  ################################################################################
  pal_single         <- setNames(color_palette[1], measure0)
  legend_lbls_single <- setNames(measure0, measure0)

  ################################################################################
  # 6B) Construir ggplot para solo measure0 (línea + puntos) y suprimir warnings al imprimir
  ################################################################################
  y_var_single <- if (use_abbrev) quo(Abrev) else quo(full_name)

  Figura <- ggplot(
    cents_single,
    aes(
      x     = Value,
      y     = fct_reorder(!!y_var_single, Value),
      group = 1    # para conectar puntos con geom_line()
    )
  ) +
    geom_line(color = pal_single, size = 0.5) +
    geom_point(color = pal_single, size = 3) +
    theme_minimal() +
    labs(
      x     = "z-score",
      y     = "Nodos",
      color = "Métrica"
    ) +
    scale_color_manual(
      values = pal_single,
      labels = legend_lbls_single
    ) +
    theme(
      axis.text.y     = element_text(size = 12),
      axis.text.x     = element_text(size = 12),
      legend.text     = element_text(size = 12),
      legend.title    = element_text(size = 12),
      axis.title.y    = element_text(size = 12),
      axis.title.x    = element_text(size = 12),
      legend.position = "bottom"
    )

  # Asignar clase personalizada para suprimir advertencias al imprimir
  class(Figura) <- c("silent_gg", class(Figura))
  assign("print.silent_gg",
         function(x, ...) suppressWarnings(NextMethod()),
         envir = .GlobalEnv)

  ################################################################################
  # 7B) Devolver tabla y gráfico (caso single)
  ################################################################################
  return(
    list(
      table = cents_single,
      plot  = Figura
    )
  )
}
