\name{combine_groupBy}
\alias{combine_groupBy}
\title{Combinar Visualizaciones de Redes y Centralidades por Grupo}

\description{
La función \code{combine_groupBy} combina gráficos de redes y medidas de centralidad para diferentes grupos en una sola visualización. Esta herramienta es ideal para comparar redes y análisis de centralidad (o métricas puente) entre grupos. Además, permite personalizar el diseño, configurar la resolución de salida y guardar los resultados en formatos de alta calidad.

\strong{Características principales}:
\itemize{
\item Integra visualizaciones de redes (e.g., creadas con \code{qgraph}) y gráficos de centralidad o métricas puente en un solo diseño.
\item Permite comparar nodos y conexiones entre grupos con etiquetas claras ("A", "B", "C").
\item Incluye opciones para personalizar las proporciones, tamaños y resolución del gráfico combinado.
\item Soporta configuraciones dinámicas: incluye una columna adicional de gráficos puente cuando se proporciona un \code{bridge_plot_group}.
\item Ofrece opciones para mostrar o guardar los gráficos directamente.
}
}

\usage{
combine_groupBy(
red_group,
plot_centralidad_group,
bridge_plot_group = NULL,
output_path = "combine_three_graphs.jpg",
height = 10,
width_a = 8,
width_bc = 4.5,
dpi = 300,
show_plot = TRUE
)
}

\arguments{
\item{red_group}{Un objeto gráfico (e.g., de \code{qgraph} o \code{igraph}) que representa la red de un grupo. Se convierte en imagen raster para la visualización combinada.}
\item{plot_centralidad_group}{Un objeto gráfico, típicamente de \code{ggplot}, que muestra las métricas de centralidad o influencia esperada para los nodos en la red.}
\item{bridge_plot_group}{(Opcional) Un objeto gráfico adicional para métricas puente, como la influencia esperada entre comunidades (\code{Bridge Expected Influence}). Si es \code{NULL}, esta sección del gráfico estará vacía.}
\item{output_path}{Ruta donde se guardará el gráfico combinado, si \code{show_plot = FALSE}. Por defecto, "combine_three_graphs.jpg".}
\item{height}{Altura total del gráfico combinado en pulgadas. Por defecto, \code{10}.}
\item{width_a}{Ancho de la columna de la red en pulgadas. Por defecto, \code{8}.}
\item{width_bc}{Ancho de las columnas de centralidad o métricas puente en pulgadas. Por defecto, \code{4.5}.}
\item{dpi}{Resolución en puntos por pulgada (DPI) del gráfico guardado. Por defecto, \code{300}.}
\item{show_plot}{Lógico. Si \code{TRUE}, muestra el gráfico combinado directamente. Si \code{FALSE}, lo guarda en \code{output_path}. Por defecto, \code{TRUE}.}
}

\details{
La función realiza los siguientes pasos:
\enumerate{
\item Convierte la red (\code{red_group}) en una imagen raster para integrarla en el diseño.
\item Ajusta las proporciones de los gráficos según si \code{bridge_plot_group} es \code{NULL} o no.
\item Combina los gráficos en un diseño de dos columnas: la primera incluye la red, y la segunda las métricas de centralidad y puente, si están disponibles.
\item Permite personalizar las dimensiones y proporciones, mostrando o guardando el resultado con la resolución configurada por \code{dpi}.
}

Ideal para presentaciones y publicaciones en las que se necesita comparar redes y sus métricas en diferentes grupos.

\value{
Un gráfico combinado que integra visualizaciones de redes, centralidad y métricas puente, ya sea mostrado o guardado como archivo.
}

\examples{

Ejemplo de red y métricas centralidad
library(ggplot2)
library(qgraph)

set.seed(123)
red_group <- qgraph(matrix(runif(100, 0, 1), nrow = 10), layout = "spring")
centralidad_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
bridge_plot <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_point()

Ejecutar la función
combine_groupBy(
  red_group = red_group,
  plot_centralidad_group = centralidad_plot,
  bridge_plot_group = bridge_plot,
  output_path = "combined_plot.jpg",
  dpi = 600, # Configuración de alta resolución
  show_plot = TRUE
)
}

\author{
Dr. José Ventura-León
}


