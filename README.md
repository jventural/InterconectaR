<p align="center">
  <img src="https://github.com/jventural/InterconectaR/blob/master/InterconectaR_Logo.jpg" alt="InterconectaR" width="200" height="200"/>
</p>

<h1 align="center">InterconectaR</h1>

<p align="center">
  <strong>Tools for Network Analysis and Psychometric Insights</strong>
  <br />
  A comprehensive R package for constructing, analyzing, and visualizing psychological networks in social science research.
  <br />
  <br />
  <a href="https://joseventuraleon.com/">Author's website</a>
</p>

<p align="center">
  <img src="https://www.r-pkg.org/badges/version/InterconectaR" alt="CRAN version"/>
  <img src="https://img.shields.io/github/license/jventural/InterconectaR" alt="License"/>
  <img src="https://img.shields.io/badge/R%20%3E%3D-4.0.0-blue" alt="R version"/>
</p>

## Overview

**InterconectaR** provides an integrated toolkit for network analysis and psychometric modeling. It includes 40 functions organized around five core areas:

- **Network Estimation** -- Estimate and compare networks across groups using multiple methods
- **Centrality & Bridge Analysis** -- Calculate, compare, and visualize centrality and bridge metrics
- **Advanced Visualization** -- Publication-ready plots for networks, centrality indices, and SEM diagrams
- **Community Detection (EGA)** -- Exploratory Graph Analysis workflows with stability refinement
- **Model Evaluation & Stability** -- Bootstrap diagnostics, case-dropping stability, and performance metrics

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

## Examples

### Estimate and visualize networks by group

```r
library(InterconectaR)

# Estimate networks for each group
nets <- estimate_networks_by_group(
  data = my_data,
  group_var = "group",
  columns = item_cols,
  default = "EBICglasso"
)

# Visualize combined network and centrality panel
combine_graphs_centrality(
  Figura1_Derecha = centrality_plot,
  network = nets[[1]]$network,
  groups = community_groups,
  error_Model = error_model,
  ncol = 2,
  widths = c(0.50, 0.60)
)
```

### Centrality comparison across groups

```r
# Bridge centrality comparison
centrality_bridge_plot(
  networks_groups = list(group1_net, group2_net),
  group_names = c("Group 1", "Group 2"),
  measure = "Bridge Expected Influence (1-step)",
  communities = community_list
)
```

### Compute network scores with EGA

```r
results <- compute_netScores(
  item_prefixes = c("dep", "anx"),
  data = my_data,
  stability_threshold = 0.70,
  stability_iter = 500
)
```

## Citation

Ventura-Leon, J. (2026). *InterconectaR: Tools for Network Analysis and Psychometric Insights* [R package]. GitHub. https://github.com/jventural/InterconectaR

## Author

**Jose Ventura-Leon**
- ORCID: [0000-0003-2996-4244](https://orcid.org/0000-0003-2996-4244)
- Email: jventuraleon@gmail.com
- Web: [joseventuraleon.com](https://joseventuraleon.com/)

## License

GPL (>= 3)
