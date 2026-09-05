# InterconectaR 1.0.1

* New `run_netpowerlab()`: launches the NetPowerLab Shiny application, which plans the sample
  size of a network study by wrapping `bootnet::netSimulator()` and `powerly::powerly()`.
  The reference network is built block by block (one block per instrument) and acts as the
  effect size; the app also writes the reproducible script and a draft of the Participants
  section. `shiny`, `bslib` and `powerly` moved into Suggests.

# InterconectaR 1.0.0

* Initial CRAN release.
* 40 functions for network analysis and psychometric modeling.
* Tools for centrality calculation, bridge metrics, and community structure visualization.
* Support for group network comparison and Mixed Graphical Models (MGM).
* Exploratory Graph Analysis (EGA) integration via EGAnet.
* Bootstrap evaluation of network estimation algorithms.
* Advanced visualization functions for network plots with R2 rings, Cleveland dot plots, and bullet charts.
