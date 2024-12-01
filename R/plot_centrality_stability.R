plot_centrality_stability <- function(caseDroppingBoot, nonParametricBoot,
                                      statistics = "bridgeStrength",
                                      output_path = "Output/Figura_2.jpg",
                                      height = 5, width = 9, dpi = 600,
                                      labels = FALSE) {
  # Verificar e instalar librerías necesarias
  required_packages <- c("ggplot2", "ggpubr", "scales")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Crear el primer gráfico (p1)
  p1 <- plot(caseDroppingBoot, statistics = statistics, labels = labels) +
    ggplot2::scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.10),
                                labels = scales::number_format(accuracy = 0.01))

  # Crear el segundo gráfico (p2)
  p2 <- plot(nonParametricBoot, labels = labels, order = "sample", statistics = "edge")

  # Combinar los gráficos
  p12 <- ggpubr::ggarrange(p1, p2, ncol = 2, labels = c("A", "B"))

  # Guardar el gráfico combinado en alta calidad
  ggsave(filename = output_path, plot = p12, height = height, width = width, dpi = dpi)

  message("Figure saved at: ", output_path)

  # Retornar el objeto combinado para inspección opcional
  return(p12)
}
