# InterconectaR 1.0.1

First version submitted to CRAN.

* `plot_networks_by_group()` restores `par()` with an immediate `on.exit()` call and always
  closes its temporary device; the NetPowerLab app restores the user's `par()` after the plot.

* The SemPowerLab application (`run_sempowerlab()`), added during development, moved to the
  PsyMetricTools package, which covers latent variable models. `semPower` is no longer
  suggested.

## New functions

* New `run_netpowerlab()`: launches the NetPowerLab Shiny application, which plans the sample
  size of a network study by wrapping `bootnet::netSimulator()` and `powerly::powerly()`.
  The reference network is built block by block (one block per instrument) and acts as the
  effect size; the app also writes the reproducible script and a draft of the Participants
  section. `shiny`, `bslib` and `powerly` moved into Suggests.
* New `sample_size_EGA_montecarlo()`, with `sample_size_EGA_control()`,
  `make_likert_thresholds()`, `plot_sample_size_EGA_montecarlo()`, `plot_power_curve_ci()`,
  `plot_failure_decomposition()`, `show_recommendation()` and `show_plots()`: a priori sample
  size for EGA through Monte Carlo simulation.

## Bug fixes

* NetPowerLab ran powerly with an increasing curve for specificity, which falls as N grows. It
  now reports a recommendation only when powerly converged and the value is not at the top of
  the range, and the script and paragraph follow the network and settings actually simulated.
* `sample_size_EGA_control()` merged nested lists: `control = list(targets = list(ari = .80))`
  silently kept the default `edge_correlation` target. Entries now replace the defaults.
* `sample_size_EGA_montecarlo()`: items that EGA leaves without a community no longer inflate
  the ARI; bootstrap resamples that never reach the target are treated as censored instead of
  dropped, so the upper CI bound is no longer understated; the bootstrap CI is reproducible
  with `parallel = TRUE`; a non positive definite partial network stops with a message that
  gives the admissible edge weight; the sensitivity sweep skips the edge-threshold runs when no
  target depends on them and compares every row with the main run.
* `optimize_leiden_resolution()` treated untested resolutions (after an early stop) as having
  TEFI = 0, could choose one of them as the best, and misaligned the models with their rows in
  `comparison` when a resolution failed.
* `plot_lvm()` labelled residual correlations as residual partial correlations, and did not
  restore the graphical parameters.
* `plot_latent_network_diagram()` called `getmatrix()` without importing it, so psychonetrics
  models failed unless psychonetrics was attached.
* `compute_netScores()` wrote to the global environment when the stability step failed.
* `convert_EGA_to_df()` returned `NA` as the algorithm for EGAnet 2.x results.
* `mgm_error_metrics()` passed `levels` instead of `level` to `mgm::mgm()`.

## CRAN compliance

* No function assigns into the global environment: the `print` methods for `silent_gg` and
  `silent_plot` are now registered by the package.
* Nothing is written to disk by default: `plot_latent_network_diagram()` requires
  `output_path`.
* Functions report progress with `message()` instead of `cat()`, and `compute_netScores()` only
  asks for dimension names in interactive sessions.
* No fixed seeds inside functions: `boot_and_evaluate()`, `refine_items_by_stability()` and
  `compute_netScores()` default to `seed = NULL`; the user's `future` plan is restored on exit;
  parallel defaults use at most 2 cores.
* Every exported function documents its return value and has a runnable example.
* `reshape2` and `stringr` are no longer needed; `viridis` moved to Suggests; `psychonetrics`
  added to Suggests. Requires R >= 4.1.0.

# InterconectaR 1.0.0

* Initial release (GitHub only).
* 40 functions for network analysis and psychometric modeling.
* Tools for centrality calculation, bridge metrics, and community structure visualization.
* Support for group network comparison and Mixed Graphical Models (MGM).
* Exploratory Graph Analysis (EGA) integration via EGAnet.
* Bootstrap evaluation of network estimation algorithms.
* Advanced visualization functions for network plots with R2 rings, Cleveland dot plots, and bullet charts.
