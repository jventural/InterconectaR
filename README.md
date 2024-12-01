<p align="center">
  <img src="https://github.com/jventural/InterconectaR/blob/master/Logo_InterconectaR.png" alt="InterconectaR" width="200" height="200"/>
</p>

<h1 align="center">InterconectaR</h1>

<p align="center">
    Una paquetería diseñada para facilitar análisis de redes, centralidad y correlaciones complejas con herramientas visuales e integradas.
    <br />
    <a href="https://joseventuraleon.com/"><strong>Explorar la página web del autor »</strong></a>
    <br />
    <br />
</p>

<!-- BADGES -->
<p align="center">
  <!-- Si tienes badges de CRAN, puedes incluirlos así: -->
  <img src="https://www.r-pkg.org/badges/version/InterconectaR" alt="CRAN version"/>
</p>


## Installation
You can install the latest version of InterconectaR from GitHub with the help of the devtools package:
```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/InterconectaR")
```

# Examples
### Algunas de las funciones de la librería InterconectaR:

---

## Combine Graphs and Centrality Analysis
Esta función combina un gráfico de centralidad generado con la librería `qgraph` y un gráfico comparativo creado con `ggplot2`. Permite guardar la visualización combinada como un archivo de alta resolución o mostrarla directamente en la ventana de gráficos de RStudio.

```r
combine_graphs_centrality(Figura1_Derecha = Figura1_Derecha, 
                          network = network, 
                          groups = groups, 
                          error_Model = error_Model, 
                          ncol = 2, 
                          widths = c(0.50, 0.60), 
                          output_path = "Output/Figura_1_Final.jpg", 
                          show_plot = TRUE)
```

---

## Calculate Centrality Measures, Standard Deviation, and Correlation
Esta función calcula la desviación estándar de las variables en un conjunto de datos y las combina con medidas de centralidad proporcionadas. Luego, realiza un análisis de correlación entre las medidas de centralidad y las desviaciones estándar.

```r
result <- calculate_centrality_sd_correlation(Data = df_new2, Centralitys = Centralitys)
print(result)
```

---

## Centrality Plot Comparison
Genera gráficos comparativos de medidas de centralidad, como `Expected Influence` y `Bridge Expected Influence`, utilizando la librería `ggplot2`. Estos gráficos ayudan a identificar diferencias en la relevancia de nodos dentro de una red.

```r
result <- centrality_plots(qgraph_obj = qgraph_obj, 
                           network = network, 
                           measure0 = "ExpectedInfluence", 
                           measure1 = "Bridge Expected Influence (1-step)")
print(result$plot)
```
