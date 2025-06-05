plot_centrality_stability <- function(caseDroppingBoot, nonParametricBoot,
                                       statistics = "bridgeStrength",
                                       labels = FALSE) {
  # Verificar e instalar librerías necesarias
  required_packages <- c("ggplot2", "patchwork", "scales")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # 1) Crear p1 y añadir scale_y sin importar si ya existe—luego lo silenciaremos al imprimir
  p1 <- plot(caseDroppingBoot, statistics = statistics, labels = labels) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.10),
      labels = scales::number_format(accuracy = 0.01)
    )

  # 2) Crear p2
  p2 <- plot(nonParametricBoot, labels = labels, order = "sample", statistics = "edge")

  # 3) Combinar usando patchwork
  combinado <- (p1 + p2) +
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(tag_levels = "A")

  # 4) Asignar clase personalizada y definir print.silent para suprimir ese warning al imprimir
  class(combinado) <- c("silent_plot", class(combinado))
  assign("print.silent_plot",
         function(x, ...) suppressWarnings(NextMethod()),
         envir = .GlobalEnv)

  # 5) Devolver el objeto combinado
  return(combinado)
}
