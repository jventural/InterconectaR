## Resubmission

This is a resubmission. In response to the review of 2026-10-02 (Leonore Hochhauser):

* Title and Description no longer start with "Tools for" or similar. The Title is now
  "Psychological Network Analysis and Psychometric Insights" and the Description starts
  with "Estimates psychological networks by group".
* R/plot_networks_by_group.R: the call to par() now stores the old settings and resets them
  with an immediate on.exit() call, inside a helper that also closes the temporary device.
* inst/shiny/netpowerlab/app.R: the user's par() is stored and reset explicitly with
  par(oldpar) after the plot (and through on.exit() if the plot fails).

## Submission summary

New submission of InterconectaR (version 1.0.1). The package provides tools for
psychological network analysis: network estimation by group, centrality and
bridge centrality summaries and plots, bootstrap stability diagnostics, item
refinement by the stability of exploratory graph analysis (EGA), and an a priori
sample size for EGA through Monte Carlo simulation.

## Test environments

* Local: Windows 11 x64, R 4.4.1 (R CMD check --as-cran --run-donttest,
  PDF manual built with pdflatex)
* win-builder: R Under development (unstable) (2026-09-21 r90579 ucrt)

## R CMD check results

win-builder (R-devel): 0 errors | 0 warnings | 1 note

* NOTE: "New submission". Expected for a first submission.
* The same note lists "Possibly misspelled words in DESCRIPTION": Borsboom,
  Epskamp and Golino are the surnames of the authors of the cited methods,
  and EGA is the acronym of exploratory graph analysis, spelled out in the
  same sentence. They are correct.

Local (R 4.4.1): 0 errors | 0 warnings | 3 notes

* NOTE: "New submission" (as above).
* NOTE: "Imports includes 30 non-default packages". The package wraps the
  estimation, bootstrap and plotting tools of the psychological network
  ecosystem (bootnet, qgraph, EGAnet, networktools, mgm) and combines their
  output in publication figures (ggplot2, patchwork, cowplot, ggforce). Packages
  used by a single optional feature (psychonetrics, viridis, shiny, bslib,
  powerly) are in Suggests and are checked with requireNamespace().
* NOTE: "unable to verify current time". Local environment only; unrelated to
  the package.

## Notes for the reviewer

* All examples are self-contained and use simulated data. Slow examples are
  wrapped in \donttest{} and use small numbers of bootstrap iterations and at
  most one core. The Shiny application (run_netpowerlab()) is only launched
  inside if (interactive()).
* No function writes to the user's file space by default:
  plot_latent_network_diagram() requires an explicit output_path, and
  sample_size_EGA_montecarlo() only writes when output_dir is supplied.
* Functions do not set a fixed seed; reproducibility is optional through a
  seed argument that defaults to NULL. The user's par() and future plan are
  restored on exit.

## Downstream dependencies

There are currently no downstream dependencies.
