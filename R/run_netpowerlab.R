#' @title Launch the NetPowerLab Shiny Application
#' @description Opens an interactive application to plan the sample size of a
#'   psychological network study (Gaussian graphical model). The reference
#'   network is declared block by block, one block per instrument, and acts as
#'   the effect size of the design. The application then wraps the two
#'   simulation approaches: `bootnet::netSimulator()`, which reports how much of
#'   the true network is recovered at each sample size (sensitivity,
#'   specificity and the correlation between estimated and true edge weights),
#'   and `powerly::powerly()`, which recommends the sample size needed to reach
#'   a declared performance with a given probability. It also writes the
#'   reproducible R script and a draft of the Participants section.
#' @param launch.browser Logical. Open the application in the default browser
#'   (default TRUE).
#' @param port Port to run the application on. `NULL` (default) lets Shiny pick
#'   a free one.
#' @param ... Further arguments passed to [shiny::runApp()].
#' @return Invisibly `NULL`. Called for its side effect: the running app.
#' @details The application needs `shiny`, `bslib` and `powerly`, which are
#'   suggested rather than required by InterconectaR: install them with
#'   `install.packages(c("shiny", "bslib", "powerly"))` the first time.
#'   Both simulations are computationally intensive; the app distributes them
#'   across cores (`nCores` in bootnet, `cores` in powerly), which turns
#'   minutes into seconds.
#' @references
#'   Constantin, M. A., Schuurman, N. K., & Vermunt, J. K. (2026). A general
#'   Monte Carlo method for sample size analysis in the context of network
#'   models. *Psychological Methods, 31*(3), 385-405.
#'   \doi{10.1037/met0000555}
#'
#'   Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological
#'   networks and their accuracy: A tutorial paper. *Behavior Research Methods,
#'   50*(1), 195-212. \doi{10.3758/s13428-017-0862-1}
#' @examples
#' \dontrun{
#' # opens the app in the browser
#' run_netpowerlab()
#'
#' # on a fixed port, without opening a browser window
#' run_netpowerlab(launch.browser = FALSE, port = 7799)
#' }
#' @export
run_netpowerlab <- function(launch.browser = TRUE, port = NULL, ...) {

  faltan <- character(0)
  for (p in c("shiny", "bslib", "powerly", "bootnet", "qgraph", "ggplot2")) {
    if (!requireNamespace(p, quietly = TRUE)) faltan <- c(faltan, p)
  }
  if (length(faltan)) {
    stop("NetPowerLab needs ", paste(sprintf("'%s'", faltan), collapse = ", "),
         ". Install with: install.packages(c(",
         paste(sprintf('"%s"', faltan), collapse = ", "), "))", call. = FALSE)
  }

  ruta <- system.file("shiny", "netpowerlab", package = "InterconectaR")
  if (!nzchar(ruta) || !file.exists(file.path(ruta, "app.R"))) {
    stop("The NetPowerLab application was not found inside the installed ",
         "package. Reinstall InterconectaR.", call. = FALSE)
  }

  shiny::runApp(appDir = ruta, launch.browser = launch.browser,
                port = port, ...)
  invisible(NULL)
}
