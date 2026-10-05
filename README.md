<p align="center">
  <img src="https://github.com/jventural/InterconectaR/blob/master/InterconectaR_Logo.jpg" alt="InterconectaR" width="200" height="200"/>
</p>

<h1 align="center">InterconectaR</h1>

<p align="center">
  <strong>Psychological Network Analysis and Psychometric Insights</strong>
  <br />
  A comprehensive R package for constructing, analyzing, and visualizing psychological networks in social science research.
  <br />
  <br />
  <a href="https://joseventuraleon.com/">Author's website</a>
</p>

<p align="center">
  <img src="https://www.r-pkg.org/badges/version/InterconectaR" alt="CRAN version"/>
  <img src="https://img.shields.io/github/license/jventural/InterconectaR" alt="License"/>
  <img src="https://img.shields.io/badge/R%20%3E%3D-4.1.0-blue" alt="R version"/>
</p>

## Overview

**InterconectaR** provides an integrated toolkit for network analysis and psychometric modeling. It includes 49 functions organized around six core areas:

- **Network Estimation** -- Estimate and compare networks across groups using multiple methods
- **Centrality & Bridge Analysis** -- Calculate, compare, and visualize centrality and bridge metrics
- **Advanced Visualization** -- Publication-ready plots for networks, centrality indices, and SEM diagrams
- **Community Detection (EGA)** -- Exploratory Graph Analysis workflows with stability refinement
- **Model Evaluation & Stability** -- Bootstrap diagnostics, case-dropping stability, and performance metrics
- **Sample Size Planning** -- Monte Carlo sample size for EGA and an interactive application for network designs

## Installation

Install the latest version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jventural/InterconectaR")
```

## Main Functions

### Network Estimation

| Function | Description |
|---|---|
| `estimate_networks_by_group()` | Estimate network models separately for each group in the data |
| `run_EGA_combinations()` | Run Exploratory Graph Analysis across multiple parameter combinations |
| `optimize_leiden_resolution()` | Optimize the resolution parameter for Leiden community detection |

### Centrality & Bridge Analysis

| Function | Description |
|---|---|
| `calculate_centrality_sd_correlation()` | Correlate centrality indices with variable standard deviations |
| `centrality_bridge_plot()` | Comparative bridge centrality plot for two groups |
| `centrality_bullet_plot()` | Bullet chart for centrality indices |
| `centrality_cleveland_plot()` | Cleveland dot plot for centrality metrics |
| `centrality_duo_plot()` | Side-by-side comparison of two centrality measures |
| `centrality_plots2()` | Centrality plot with optional bridge metrics |
| `plot_centrality_by_group()` | Plot centrality measures by group |

### Visualization

| Function | Description |
|---|---|
| `plot_net()` | Network plot with R2 progress rings |
| `plot_net_group()` | Two networks side by side with R2 progress rings |
| `plot_networks_by_group()` | Combined network panel by group |
| `plot_latent_network_diagram()` | SEM path diagram for latent network models |
| `plot_lvm()` | Latent variable model visualization |
| `combine_graphs_centrality()` | Combine qgraph network with centrality line plot |
| `combine_graphs_centrality2()` | Combine network and centrality graphs (version 2) |
| `combine_groupBy()` | Combine network, centrality, and bridge plots into labeled panel |
| `qgraph_centrality_panel()` | Combined panel with qgraph network and centrality plot |

### Community Detection (EGA)

| Function | Description |
|---|---|
| `compute_netScores()` | Compute network scores with stability analysis using EGA |
| `convert_EGA_to_df()` | Extract network metrics from a single EGA result |
| `convert_EGA_list_to_df()` | Convert a list of EGA results into a combined data frame |
| `refine_items_by_stability()` | Iteratively refine items by bootstrap stability |

### Model Evaluation & Stability

| Function | Description |
|---|---|
| `boot_and_evaluate()` | Bootstrap and evaluate network analysis |
| `filter_correlation_stability()` | Extract and filter correlation stability indices |
| `plot_centrality_stability()` | Plot centrality stability diagnostics |
| `summarise_case_drop_stability()` | Summarise case-dropping bootstrap stability |
| `summarise_nonparametric_edges()` | Summarise nonparametric edge bootstrap results |
| `summary_metrics()` | Summarize network performance metrics |
| `plot_avg_TEFI()` | Plot average TEFI by sample size |
| `plot_performance_metrics()` | Plot network performance metrics across conditions |
| `process_LCT()` | Process Loadings Comparison Test results |

### Utilities

| Function | Description |
|---|---|
| `Density_report()` | Calculate and report network density statistics |
| `get_edge_weights_summary()` | Descriptive statistics for network edge weights |
| `mgm_error_metrics()` | Fit a Mixed Graphical Model and extract prediction errors |
| `mgm_errors_groups()` | MGM error metrics by group |
| `net_reduce2()` | Reduce redundant node pairs using PCA or goldbricker selection |
| `choose_best_method()` | Select the best estimation method based on correlation analysis |
| `structure_groups()` | Structure group labels for network communities |

### Sample Size Planning for EGA

| Function | Description |
|---|---|
| `sample_size_EGA_montecarlo()` | A priori sample size for EGA through Monte Carlo simulation |
| `sample_size_EGA_control()` | Advanced settings of the simulation (targets, thresholds, algorithm) |
| `make_likert_thresholds()` | Likert thresholds with floor or ceiling effects for the simulated items |
| `plot_sample_size_EGA_montecarlo()` | Recovery metrics across the candidate sample sizes |
| `plot_power_curve_ci()` | Power curve with its bootstrap band and the recommended n |
| `plot_failure_decomposition()` | Which constraint fails at each sample size |
| `show_recommendation()`, `show_plots()` | One-line recommendation and the stored plots |

### Interactive Application

| Function | Description |
|---|---|
| `run_netpowerlab()` | Launch NetPowerLab: plan the sample size of a network study with `bootnet::netSimulator()` and `powerly::powerly()` |

```r
InterconectaR::run_netpowerlab()
```

SemPowerLab, the companion application for designs with latent variables, is part of the
[PsyMetricTools](https://github.com/jventural/PsyMetricTools) package (`run_sempowerlab()`).

The reference network is declared block by block, one block per instrument, and acts as the
effect size of the design. The app reports what is recovered at each sample size (sensitivity,
specificity and the correlation between estimated and true weights), recommends the sample size
that reaches a declared performance, and writes both the reproducible script and a draft of the
Participants section. It needs `shiny`, `bslib` and `powerly`, which are suggested rather than
required: `install.packages(c("shiny", "bslib", "powerly"))`.

## Examples

The examples use simulated data: eight items that measure two correlated factors, answered by
two groups.

```r
library(InterconectaR)

set.seed(123)
n <- 300
f1 <- rnorm(n)
f2 <- 0.3 * f1 + rnorm(n)
items <- data.frame(
  sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
  sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
)
names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
items$sex <- rep(c("Female", "Male"), each = n / 2)
```

### Estimate and visualize networks by group

```r
# Estimate networks for each group
nets <- estimate_networks_by_group(
  data = items,
  group_var = "sex",
  columns = names(items)[1:8],
  default = "EBICglasso"
)

# Network of one group with its centrality plot
net <- nets$Female
g <- qgraph::qgraph(net$graph, DoNotPlot = TRUE)
cent <- centrality_plots2(g, net,
                          groups = rep(c("A", "B"), each = 4),
                          measure1 = "Bridge Expected Influence (1-step)")
r2 <- mgm_error_metrics(items[items$sex == "Female", 1:8],
                        type = rep("g", 8), level = rep(1, 8))$R2

combine_graphs_centrality(
  Figura1_Derecha = cent$plot,
  network = net,
  groups = list(A = 1:4, B = 5:8),
  error_Model = r2
)
```

### Centrality comparison across groups

```r
# Bridge centrality comparison
centrality_bridge_plot(
  networks_groups = nets,
  group_names = c("Female", "Male"),
  measure = "Bridge Expected Influence (1-step)",
  communities = rep(c("A", "B"), each = 4)
)
```

### Compute network scores with EGA

```r
results <- compute_netScores(
  item_prefixes = c("A", "B"),
  data = items,
  stability_threshold = 0.70,
  stability_iter = 500,
  rename_dims = FALSE
)
```

### A priori sample size for EGA

```r
res <- sample_size_EGA_montecarlo(
  community_sizes = c(4, 4),
  within_edge = 0.20,
  sample_sizes = c(150, 250, 400, 600),
  n_rep = 500,
  seed = 2026
)
res
plot_power_curve_ci(res)
```

## Citation

Ventura-Leon, J. (2026). *InterconectaR: Psychological Network Analysis and Psychometric Insights* [R package]. GitHub. https://github.com/jventural/InterconectaR

## Author

**Jose Ventura-Leon**
- ORCID: [0000-0003-2996-4244](https://orcid.org/0000-0003-2996-4244)
- Email: jventuraleon@gmail.com
- Web: [joseventuraleon.com](https://joseventuraleon.com/)

## License

GPL (>= 3)
