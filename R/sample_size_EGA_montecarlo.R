#' A Priori Sample Size Estimation for EGA Using Monte Carlo Simulation
#'
#' @description
#' Estimates an a priori sample size for psychometric network designs using
#' Monte Carlo simulation. Data are generated from a population correlation
#' matrix or from a hypothesized partial correlation network, then
#' \code{EGAnet::EGA()} is fitted across candidate sample sizes.
#'
#' When the input is a correlation matrix, the implied partial correlation
#' network is computed and used as the ground truth for edge-recovery metrics
#' (since \code{EGAnet::EGA()} returns a partial correlation network).
#'
#' Optionally runs in parallel using \code{future.apply} with a per-replication
#' progress bar via \code{progressr}.
#'
#' @param community_sizes Integer vector with the number of items per
#'   community (e.g. \code{c(4, 4, 4, 4)} for a 4-community scale with 4
#'   items each). When set, the function builds the population partial-
#'   correlation network and the community assignment internally;
#'   \code{population_network} and \code{communities} can be left at
#'   \code{NULL}. This is the recommended simple API.
#' @param within_edge Partial-correlation weight assigned to every within-
#'   community edge when the network is built internally. Default \code{0.10}
#'   (a realistic, weak-loading scenario consistent with Christensen et al.,
#'   2024). Ignored if \code{population_network} is supplied.
#' @param bridges Optional \code{data.frame} (or matrix coercible to one) with
#'   columns \code{i}, \code{j}, \code{weight} describing inter-community
#'   bridge edges (1-based item indices). Default \code{NULL} = no bridges.
#' @param community_names Optional character vector of community labels used
#'   when building \code{communities} internally. Default \code{NULL} =
#'   \code{c("F1", "F2", ...)}.
#' @param population_network Optional square matrix supplied directly
#'   (advanced API): either the population correlation matrix
#'   (\code{network_type = "correlation"}) or the hypothesized partial
#'   correlation network (\code{network_type = "partial"}). When provided,
#'   \code{communities} must also be provided and \code{factor_sizes} is
#'   ignored.
#' @param communities Expected community assignment when using the advanced
#'   API. Either a named list of item indices per dimension or a vector with
#'   one community label per item.
#' @param sample_sizes Numeric vector with candidate sample sizes. Default
#'   \code{c(150, 250, 400, 600, 900, 1300)} brackets the typical EGA range
#'   reported in the literature (Christensen, Garrido, Guerra-Pena & Golino,
#'   2024; Golino et al., 2020).
#' @param n_rep Number of Monte Carlo replications per sample size. Default
#'   \code{500} (500-1000 is the working range used in published EGA
#'   simulations).
#' @param network_type \code{"partial"} (default, recommended) or
#'   \code{"correlation"}.
#' @param data_type \code{"ordinal"} (default, the standard in psychometric
#'   scales) or \code{"continuous"}.
#' @param power_target Numeric in (0, 1). Target probability for the combined
#'   power curve. Default \code{0.80} (Cohen-style convention).
#' @param validation_n_rep Number of additional replications at the recommended
#'   \code{n} for empirical power validation. Set to \code{0} (default) to
#'   skip; \code{500} is a good value when reporting in a paper.
#' @param sensitivity Logical. If \code{TRUE} the function automatically runs a
#'   sensitivity sweep over the most influential arbitrary choices of the
#'   pipeline: \code{true_edge_threshold} (0.01 / 0.05 / 0.10),
#'   \code{edge_threshold_method} ("absolute" vs "match_true"),
#'   \code{combination} ("and" vs "primary") and, for ordinal data, a floor
#'   pattern of thresholds. The two edge-threshold sweeps only run when a
#'   target depends on the edge thresholds (sensitivity, specificity,
#'   precision, fdr, edge_correlation_true_edges or mae_true_edges). Each row
#'   is compared with the recommendation of the main run. Results are stored
#'   in \code{$sensitivity}. Default \code{FALSE}.
#' @param plots Logical. If \code{TRUE}, generates the standard plots
#'   (\code{plot_power_curve_ci}, \code{plot_failure_decomposition},
#'   \code{plot_sample_size_EGA_montecarlo} and the bootstrap distribution of
#'   \code{n*}). Plots are stored in \code{$plots} and saved to disk if
#'   \code{output_dir} is set. Default \code{FALSE}.
#' @param output_dir Optional path to a directory. When set, the function
#'   creates the directory if needed and writes \code{result.rds},
#'   \code{summary.csv}, \code{sensitivity.csv} (if applicable) and PNGs of all
#'   plots. Default \code{NULL} (no files written).
#' @param parallel Logical; run replications in parallel via
#'   \code{future.apply::future_lapply()}. Default \code{FALSE}.
#' @param n_workers Optional integer with the number of parallel workers when
#'   \code{parallel = TRUE}. Default \code{min(8L, future::availableCores() - 1L)}.
#'   The cap at 8 is deliberate: on Windows each \code{future::multisession}
#'   worker spawns a fresh Rscript that re-loads EGAnet (~400 MB RAM and
#'   several seconds of startup each). Going wider routinely freezes the
#'   user-visible progress bar at "0-7%" while the workers initialise.
#'   Override explicitly (e.g. \code{n_workers = 11} on a 12-core box) if you
#'   have ~5 GB of free RAM and want the extra throughput.
#' @param seed Optional random seed. With \code{parallel = TRUE} the seed is
#'   forwarded to \code{future.seed} for reproducible parallel RNG.
#' @param verbose Logical; print progress messages and show a progress bar.
#' @param control Optional named list to override advanced defaults. See
#'   \code{\link{sample_size_EGA_control}} for the full list of tunable
#'   parameters and their literature-backed defaults (categories, corr, model,
#'   algorithm, uni.method, criteria, targets, recommendation_method, boots,
#'   require_convergence, require_correct_dimensions, combination,
#'   primary_metric, true_edge_threshold, estimated_edge_threshold,
#'   edge_threshold_method, thresholds).
#' @param ... Additional arguments passed to \code{EGAnet::EGA()}.
#'
#' @return A list with class \code{sample_size_EGA_montecarlo} containing the
#'   recommended sample size, summary table (with median, mean, 95 percent
#'   Monte Carlo bands and per-metric power curves), raw replication results,
#'   criteria, targets, isotonic interpolation, bootstrap distribution and CI
#'   for the recommended \code{n}, validation results (when requested), the
#'   population correlation matrix, the true partial-correlation network used
#'   as ground truth, and the expected communities.
#'
#' @section Bootstrap interpretation:
#' The 95 percent CI returned in \code{recommended_n_ci} (and the band
#' available via \code{plot_power_curve_ci()}) captures Monte Carlo
#' uncertainty of the recommended-\code{n} estimator \emph{conditional on the
#' assumed population model and the chosen pipeline}. It is a within-model
#' uncertainty band, not a population-level inferential statement. In
#' particular, it does NOT account for: (1) misspecification of
#' \code{population_network} (the wrong "truth"); (2) departures from the
#' simulated data-generating process such as real ordinal scales with
#' floor / ceiling effects, missingness, or non-normality; or (3) differences
#' between the chosen EGA pipeline (\code{corr}, \code{model},
#' \code{algorithm}) and what users will actually run on their data.
#'
#' For robust reporting, complement the CI with sensitivity analyses: vary
#' the population network (denser or sparser), \code{edge_threshold_method},
#' \code{combination}, \code{true_edge_threshold} (e.g. 0.01 vs 0.05 vs 0.10,
#' usually the most influential arbitrary choice in the pipeline when the
#' targets include edge-recovery metrics), and \code{thresholds} (use
#' \code{make_likert_thresholds()} to build floor / ceiling patterns). The
#' \code{validation} list provides an empirical check that the recommended
#' \code{n} actually delivers the target power on a fresh batch of
#' replications.
#'
#' @section Partial-to-Sigma convention:
#' When \code{network_type = "partial"}, the implied population correlation
#' matrix \code{Sigma} is reconstructed from the partial-correlation network
#' \code{P} by setting the precision matrix to \code{K = I - P} (unit
#' residual variances on the diagonal, off-diagonal \code{-P}), inverting to
#' obtain a covariance, and rescaling to a correlation. This is one valid
#' parameterisation but \emph{not} the only one: different residual-variance
#' choices are compatible with the same partial structure and produce
#' different \code{Sigma}, different simulated data, and ultimately different
#' \code{recommended_n} values. The recommendation is therefore conditional
#' on this generative convention. Report it explicitly in the manuscript and,
#' if the partial network admits a natural alternative scaling, run a
#' sensitivity analysis under that alternative.
#'
#' @section Edge-recovery metrics under \code{network_type = "correlation"}:
#' When \code{network_type = "correlation"} the implied true partial network
#' is computed by inverting \code{Sigma} and is typically dense. Because
#' \code{model = "glasso"} produces sparse estimates, binary edge-recovery
#' metrics (\code{sensitivity}, \code{specificity}, \code{precision},
#' \code{fdr}) become \emph{structurally biased} regardless of sample size:
#' the design itself imposes a sparsity mismatch. In this regime, prefer
#' targets based on \code{ari} (community recovery) or
#' \code{edge_correlation} (continuous agreement) and treat binary edge
#' targets as untrustworthy. The function emits a warning if you combine
#' \code{network_type = "correlation"} with binary edge metrics in
#' \code{targets}.
#'
#' @export
#' @importFrom EGAnet EGA
#' @examples
#' # A toy run: two communities of four items, three candidate sample sizes
#' # and 10 replications each. Real studies need n_rep = 500 or more.
#' res <- sample_size_EGA_montecarlo(
#'   community_sizes = c(4, 4),
#'   within_edge = 0.20,
#'   sample_sizes = c(100, 200, 400),
#'   n_rep = 10,
#'   data_type = "continuous",
#'   seed = 1,
#'   verbose = FALSE,
#'   control = list(boots = 100, corr = "pearson", algorithm = "walktrap")
#' )
#' res
#' res$summary[, c("Sample_Size", "power_combined")]
sample_size_EGA_montecarlo <- function(
    community_sizes = NULL,
    within_edge = 0.10,
    bridges = NULL,
    community_names = NULL,
    population_network = NULL,
    communities = NULL,
    sample_sizes = c(150, 250, 400, 600, 900, 1300),
    n_rep = 500,
    network_type = c("partial", "correlation"),
    data_type = c("ordinal", "continuous"),
    power_target = 0.80,
    validation_n_rep = 0,
    sensitivity = FALSE,
    plots = FALSE,
    output_dir = NULL,
    parallel = FALSE,
    n_workers = NULL,
    seed = NULL,
    verbose = TRUE,
    control = list(),
    ...
) {

  network_type <- match.arg(network_type)
  data_type <- match.arg(data_type)

  api_mode <- "advanced"
  if (is.null(population_network)) {
    if (is.null(community_sizes)) {
      stop("Provide either 'community_sizes' (simple API) or 'population_network' ",
           "+ 'communities' (advanced API).")
    }
    built <- .InterconectaR_build_partial_network(
      community_sizes = community_sizes,
      within_edge     = within_edge,
      bridges         = bridges,
      community_names = community_names
    )
    population_network <- built$network
    if (is.null(communities)) communities <- built$communities
    network_type <- "partial"
    api_mode <- "simple"
  } else if (is.null(communities)) {
    stop("'communities' is required when 'population_network' is supplied directly.")
  }

  ctrl <- sample_size_EGA_control(control)

  categories               <- ctrl$categories
  thresholds               <- ctrl$thresholds
  corr                     <- ctrl$corr
  model                    <- ctrl$model
  algorithm                <- ctrl$algorithm
  uni.method               <- ctrl$uni.method
  criteria                 <- ctrl$criteria
  targets                  <- ctrl$targets
  recommendation_method    <- ctrl$recommendation_method
  boots                    <- ctrl$boots
  require_convergence      <- ctrl$require_convergence
  require_correct_dimensions <- ctrl$require_correct_dimensions
  combination              <- ctrl$combination
  primary_metric           <- ctrl$primary_metric
  true_edge_threshold      <- ctrl$true_edge_threshold
  estimated_edge_threshold <- ctrl$estimated_edge_threshold
  edge_threshold_method    <- ctrl$edge_threshold_method

  # Fix B5: optionally enforce a symmetric edge-existence cutoff so that the
  # ground-truth and the estimated network are evaluated under the same rule.
  if (edge_threshold_method == "match_true") {
    estimated_edge_threshold <- true_edge_threshold
  }

  if (network_type == "correlation") {
    warning(
      "When 'network_type = \"correlation\"', the implied true partial network ",
      "is computed via solve(sigma) and is typically dense. EGA with glasso ",
      "estimates a sparse network, which biases binary edge-recovery metrics ",
      "(sensitivity, specificity, FDR, precision). Prefer 'network_type = ",
      "\"partial\"' with an explicit sparse partial-correlation matrix when ",
      "edge recovery is the focus.",
      call. = FALSE
    )
  }
  if (!is.numeric(power_target) || length(power_target) != 1 ||
      power_target <= 0 || power_target >= 1) {
    stop("'power_target' must be a single numeric in (0, 1).")
  }
  if (!is.numeric(boots) || length(boots) != 1 || boots < 1) {
    stop("'boots' must be a single positive integer.")
  }
  boots <- as.integer(boots)
  if (!is.numeric(validation_n_rep) || length(validation_n_rep) != 1 || validation_n_rep < 0) {
    stop("'validation_n_rep' must be a non-negative integer.")
  }
  validation_n_rep <- as.integer(validation_n_rep)

  if (is.null(targets)) {
    pure_power <- c("convergence_rate", "p_dimensions")
    targets <- criteria[!names(criteria) %in% pure_power]
  }
  unknown_targets <- setdiff(names(targets), names(.InterconectaR_metric_directions()))
  if (length(unknown_targets) > 0) {
    warning("Ignoring unknown targets: ", paste(unknown_targets, collapse = ", "),
            ". Allowed: ", paste(names(.InterconectaR_metric_directions()), collapse = ", "),
            ".", call. = FALSE)
  }

  # Fix B7: validate primary_metric when combination = "primary".
  if (combination == "primary") {
    if (length(targets) == 0) {
      stop("'combination = \"primary\"' requires at least one entry in 'targets'.")
    }
    if (is.null(primary_metric)) {
      primary_metric <- names(targets)[1]
    } else if (!primary_metric %in% names(targets)) {
      stop("'primary_metric' must be one of the names in 'targets': ",
           paste(names(targets), collapse = ", "))
    }
    # Council fix: combining the laxer "primary" rule with disabled gates can
    # produce silently optimistic recommendations. Warn loudly so the user
    # confirms this is intentional (e.g. a deliberate sensitivity analysis).
    if (!isTRUE(require_convergence) || !isTRUE(require_correct_dimensions)) {
      warning(
        "Combination 'primary' with require_convergence = ",
        isTRUE(require_convergence),
        " and require_correct_dimensions = ",
        isTRUE(require_correct_dimensions),
        " disables the safety gates. The recommendation may be optimistic ",
        "because replications that did not converge or did not recover the ",
        "expected number of dimensions can still count toward power. Use this ",
        "combination only as an exploratory sensitivity analysis.",
        call. = FALSE
      )
    }
  }

  # Council fix: binary edge-recovery metrics are structurally biased when
  # the ground-truth partial network is computed from a correlation matrix
  # (typically dense) and then estimated with glasso (typically sparse).
  binary_edge_metrics <- c("sensitivity", "specificity", "precision", "fdr")
  if (network_type == "correlation" &&
      length(intersect(names(targets), binary_edge_metrics)) > 0) {
    warning(
      "'targets' includes binary edge-recovery metrics (",
      paste(intersect(names(targets), binary_edge_metrics), collapse = ", "),
      ") under network_type = 'correlation'. These metrics are structurally ",
      "biased in this regime because the implied true network is dense and ",
      "glasso estimates are sparse, so the recommendation may not reflect ",
      "true sample-size requirements. Prefer 'ari' and/or 'edge_correlation' ",
      "as decisional targets in this mode, or switch to network_type = ",
      "'partial' with an explicitly sparse partial-correlation matrix.",
      call. = FALSE
    )
  }

  # Council fix: warn when thresholds are below the new defensible default
  # (0.05). Very low thresholds inflate sensitivity/specificity/FDR by
  # promoting negligible edges to "true" edges.
  if (edge_threshold_method == "absolute") {
    if (true_edge_threshold < 0.05) {
      warning(
        "'true_edge_threshold = ", true_edge_threshold, "' is below the ",
        "recommended default of 0.05; negligible edges may be promoted to ",
        "'true' edges and inflate sensitivity / specificity / FDR. Consider ",
        "running a sensitivity analysis at 0.01, 0.05 and 0.10 and reporting ",
        "how 'recommended_n' changes.",
        call. = FALSE
      )
    }
    if (estimated_edge_threshold < 0.05) {
      warning(
        "'estimated_edge_threshold = ", estimated_edge_threshold,
        "' is below the recommended default of 0.05; any non-zero estimate ",
        "counts as a detected edge, which is misleading because glasso ",
        "already shrinks small edges to or near zero.",
        call. = FALSE
      )
    }
  }

  if (is.data.frame(population_network)) {
    population_network <- as.matrix(population_network)
  }
  if (!is.matrix(population_network) || nrow(population_network) != ncol(population_network)) {
    stop("'population_network' must be a square matrix.")
  }
  if (any(!is.finite(population_network))) {
    stop("'population_network' must contain only finite values.")
  }
  if (!is.numeric(sample_sizes) || length(sample_sizes) == 0 || any(sample_sizes < 1) ||
      any(abs(sample_sizes - round(sample_sizes)) > .Machine$double.eps^0.5)) {
    stop("'sample_sizes' must be a vector of positive integers.")
  }
  sample_sizes <- as.integer(sample_sizes)
  if (!is.numeric(n_rep) || length(n_rep) != 1 || n_rep < 1) {
    stop("'n_rep' must be a single positive integer.")
  }
  n_rep <- as.integer(n_rep)
  if (data_type == "ordinal" && (!is.numeric(categories) || categories < 2)) {
    stop("'categories' must be an integer >= 2 when data_type = 'ordinal'.")
  }
  allowed_criteria <- c("convergence_rate", "p_dimensions", "ari", "edge_correlation",
                        "edge_correlation_true_edges", "mae_true_edges",
                        "sensitivity", "specificity", "precision", "fdr")
  unknown_criteria <- setdiff(names(criteria), allowed_criteria)
  if (length(unknown_criteria) > 0) {
    warning("Ignoring unknown criteria: ", paste(unknown_criteria, collapse = ", "))
  }

  p <- ncol(population_network)
  item_names <- colnames(population_network)
  if (is.null(item_names)) {
    item_names <- paste0("item", seq_len(p))
    colnames(population_network) <- rownames(population_network) <- item_names
  }

  expected_wc <- .InterconectaR_communities_to_vector(communities, item_names)
  expected_k <- length(unique(expected_wc))

  sigma <- .InterconectaR_population_to_correlation(population_network, network_type)
  colnames(sigma) <- rownames(sigma) <- item_names

  true_network <- if (network_type == "partial") {
    # same symmetrisation as the matrix used to simulate the data
    pn <- (population_network + t(population_network)) / 2
    diag(pn) <- 0
    pn
  } else {
    .InterconectaR_correlation_to_partial(sigma)
  }
  colnames(true_network) <- rownames(true_network) <- item_names

  # Council fix: characterise the true network's sparsity. Two regimes deserve
  # warnings (the user can still proceed):
  #   - very sparse (>80% zeros): edge_correlation global can be inflated by
  #     "zero matches zero". Recommend edge_correlation_true_edges / mae_true_edges.
  #   - very few true edges (<5): the true-edges-only metrics are unstable
  #     (safe_cor returns NA below 2 valid pairs), so the sweep / power curve
  #     becomes noisy. Recommend lowering true_edge_threshold or denser network.
  upper_idx        <- upper.tri(true_network)
  upper_vals       <- true_network[upper_idx]
  n_true_edges     <- sum(abs(upper_vals) > true_edge_threshold, na.rm = TRUE)
  n_possible_edges <- length(upper_vals)
  sparsity_pct     <- 1 - n_true_edges / n_possible_edges

  any_global_target <- any(c("edge_correlation") %in% names(targets))
  if (any_global_target && sparsity_pct >= 0.80) {
    warning(
      sprintf(
        "True network is sparse (%.0f%% zeros under |w| > %.2f, %d true edges of %d). ",
        100 * sparsity_pct, true_edge_threshold, n_true_edges, n_possible_edges
      ),
      "'edge_correlation' (global) can be inflated by zero-matches-zero in this ",
      "regime. Prefer 'edge_correlation_true_edges' or 'mae_true_edges' as ",
      "decisional targets via control = list(targets = list(...)).",
      call. = FALSE
    )
  }
  any_true_edges_target <- any(c("edge_correlation_true_edges", "mae_true_edges")
                               %in% names(targets))
  if (any_true_edges_target && n_true_edges < 5) {
    warning(
      sprintf(
        "Only %d true edges with |w| > %.2f. ",
        n_true_edges, true_edge_threshold
      ),
      "'edge_correlation_true_edges' / 'mae_true_edges' will be unstable (NAs ",
      "are likely). Lower true_edge_threshold, densify the population network, ",
      "or fall back to 'ari' as the primary target.",
      call. = FALSE
    )
  }

  # Council fix: Pearson correlation between two vectors is undefined when
  # either has zero variance. If all true-edge weights are equal (e.g. a 4x4
  # block-equicorrelated network with within = 0.10 and bridges below the
  # threshold), edge_correlation_true_edges returns NA on every replicate ->
  # power_combined collapses to 0 -> recommended_n is silently NA. Catch this
  # at fit-time and recommend mae_true_edges, which is well-defined under
  # constant true weights.
  if ("edge_correlation_true_edges" %in% names(targets) && n_true_edges >= 2) {
    sd_true <- stats::sd(upper_vals[abs(upper_vals) > true_edge_threshold])
    if (is.finite(sd_true) && sd_true == 0) {
      warning(
        sprintf(
          "All %d true edges have the SAME weight (SD = 0). ",
          n_true_edges
        ),
        "'edge_correlation_true_edges' is mathematically undefined here ",
        "(Pearson correlation requires non-zero variance) and will return NA ",
        "on every replicate, forcing recommended_n = NA. Use 'mae_true_edges' ",
        "instead - e.g. control = list(targets = list(ari = 0.80, ",
        "mae_true_edges = 0.05)). Alternatively lower 'true_edge_threshold' ",
        "to include weaker edges (e.g. bridges) and restore variance.",
        call. = FALSE
      )
    }
  }

  if (isTRUE(parallel)) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Install 'future' and 'future.apply' to use parallel = TRUE.")
    }
    if (is.null(n_workers)) {
      # Cap default at 8 workers: on Windows each future::multisession worker
      # spins a fresh Rscript that re-loads EGAnet (~400 MB RAM and several
      # seconds of startup each). Going wider trades a few seconds of compute
      # for minutes of spinup and tens of GB of RAM, which routinely freezes
      # the user-visible progress bar at "0-7%" while workers initialise.
      n_workers <- min(8L, max(1L, future::availableCores() - 1L))
    }
    # Only (re)set the plan if there is no multisession already active. This
    # guards the recursive sweep calls inside .InterconectaR_run_sensitivity():
    # without it, each sub-call would tear down and respawn the entire worker
    # pool, paying ~30 s of EGAnet load time N times.
    if (!inherits(future::plan(), "multisession")) {
      old_plan <- future::plan(future::multisession, workers = n_workers)
      on.exit(future::plan(old_plan), add = TRUE)
    } else {
      # an active plan is reused as is, so report its real number of workers
      n_workers <- future::nbrOfWorkers()
    }
  }

  jobs <- expand.grid(
    sample_size = sample_sizes,
    replication = seq_len(n_rep),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  total_jobs <- nrow(jobs)

  if (isTRUE(verbose)) {
    message(
      "Running ", total_jobs, " EGA fits (",
      length(sample_sizes), " sample sizes x ", n_rep, " replications)",
      if (isTRUE(parallel)) paste0(" using ", n_workers, " workers") else " sequentially",
      "."
    )
  }

  ega_extra_args <- list(...)

  worker_env <- new.env(parent = globalenv())
  worker_env$jobs <- jobs
  worker_env$sigma <- sigma
  worker_env$data_type <- data_type
  worker_env$categories <- categories
  worker_env$thresholds <- thresholds
  worker_env$corr <- corr
  worker_env$model <- model
  worker_env$algorithm <- algorithm
  worker_env$uni.method <- uni.method
  worker_env$true_network <- true_network
  worker_env$expected_wc <- expected_wc
  worker_env$expected_k <- expected_k
  worker_env$true_edge_threshold <- true_edge_threshold
  worker_env$estimated_edge_threshold <- estimated_edge_threshold
  worker_env$item_names <- item_names
  worker_env$ega_extra_args <- ega_extra_args
  worker_env$.InterconectaR_simulate_from_sigma <- .InterconectaR_simulate_from_sigma
  worker_env$.InterconectaR_resolve_thresholds  <- .InterconectaR_resolve_thresholds
  worker_env$.InterconectaR_network_metrics <- .InterconectaR_network_metrics
  worker_env$.InterconectaR_safe_ratio <- .InterconectaR_safe_ratio
  worker_env$.InterconectaR_safe_cor <- .InterconectaR_safe_cor
  worker_env$.InterconectaR_adjusted_rand_index <- .InterconectaR_adjusted_rand_index
  for (helper_name in c(".InterconectaR_simulate_from_sigma",
                        ".InterconectaR_resolve_thresholds",
                        ".InterconectaR_network_metrics",
                        ".InterconectaR_safe_ratio",
                        ".InterconectaR_safe_cor",
                        ".InterconectaR_adjusted_rand_index")) {
    environment(worker_env[[helper_name]]) <- worker_env
  }

  worker <- function(job_id, progress = NULL) {
    sample_size <- jobs$sample_size[job_id]
    replication <- jobs$replication[job_id]

    simulated_data <- .InterconectaR_simulate_from_sigma(
      n = sample_size,
      sigma = sigma,
      data_type = data_type,
      categories = categories,
      thresholds = thresholds
    )

    ega_args <- c(
      list(
        data = simulated_data,
        corr = corr,
        model = model,
        algorithm = algorithm,
        uni.method = uni.method,
        plot.EGA = FALSE,
        verbose = FALSE
      ),
      ega_extra_args
    )

    ega_result <- tryCatch(
      suppressWarnings(do.call(EGAnet::EGA, ega_args)),
      error = function(e) e
    )

    if (!is.null(progress)) progress()

    if (inherits(ega_result, "error")) {
      return(data.frame(
        Sample_Size = sample_size,
        Replication = replication,
        converged = FALSE,
        n_dimensions = NA_integer_,
        dimensions_correct = NA,
        ari = NA_real_,
        edge_correlation = NA_real_,
        edge_correlation_true_edges = NA_real_,
        mae_true_edges = NA_real_,
        sensitivity = NA_real_,
        specificity = NA_real_,
        precision = NA_real_,
        fdr = NA_real_,
        mae_global = NA_real_,
        TEFI = NA_real_,
        error = ega_result$message,
        stringsAsFactors = FALSE
      ))
    }

    estimated_network <- ega_result$network
    estimated_wc <- ega_result$wc[item_names]

    metrics <- .InterconectaR_network_metrics(
      true_network = true_network,
      estimated_network = estimated_network,
      true_edge_threshold = true_edge_threshold,
      estimated_edge_threshold = estimated_edge_threshold
    )

    data.frame(
      Sample_Size = sample_size,
      Replication = replication,
      converged = TRUE,
      n_dimensions = ega_result$n.dim,
      dimensions_correct = isTRUE(ega_result$n.dim == expected_k),
      ari = .InterconectaR_adjusted_rand_index(expected_wc, estimated_wc),
      edge_correlation = metrics$edge_correlation,
      edge_correlation_true_edges = metrics$edge_correlation_true_edges,
      mae_true_edges = metrics$mae_true_edges,
      sensitivity = metrics$sensitivity,
      specificity = metrics$specificity,
      precision = metrics$precision,
      fdr = metrics$fdr,
      mae_global = metrics$mae_global,
      TEFI = if (!is.null(ega_result$TEFI)) ega_result$TEFI else NA_real_,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  environment(worker) <- worker_env

  run_mc <- function(jobs_df, seed_offset = 0L) {
    worker_env$jobs <- jobs_df
    total <- nrow(jobs_df)
    inner <- function() {
      progress <- if (isTRUE(verbose)) progressr::progressor(steps = total) else NULL
      effective_seed <- if (is.null(seed)) TRUE else seed + seed_offset
      if (isTRUE(parallel)) {
        future.apply::future_lapply(
          seq_len(total),
          worker,
          progress = progress,
          future.seed = effective_seed,
          future.globals = FALSE,
          future.packages = "EGAnet"
        )
      } else {
        if (!is.null(seed)) set.seed(seed + seed_offset)
        lapply(seq_len(total), worker, progress = progress)
      }
    }
    if (isTRUE(verbose)) progressr::with_progress(inner()) else inner()
  }

  results_list <- run_mc(jobs)
  raw_results <- dplyr::bind_rows(results_list)

  raw_results <- .InterconectaR_compute_per_replication_meets(
    raw_results, targets,
    require_convergence        = require_convergence,
    require_correct_dimensions = require_correct_dimensions,
    combination                = combination,
    primary_metric             = primary_metric
  )

  summary <- raw_results %>%
    dplyr::group_by(.data$Sample_Size) %>%
    dplyr::summarise(
      replications = dplyr::n(),
      convergence_rate = mean(.data$converged, na.rm = TRUE),
      p_dimensions = mean(ifelse(is.na(.data$dimensions_correct), FALSE, .data$dimensions_correct)),
      median_ari = stats::median(.data$ari, na.rm = TRUE),
      mean_ari = mean(.data$ari, na.rm = TRUE),
      q025_ari = .InterconectaR_safe_quantile(.data$ari, 0.025),
      q975_ari = .InterconectaR_safe_quantile(.data$ari, 0.975),
      median_edge_correlation = stats::median(.data$edge_correlation, na.rm = TRUE),
      mean_edge_correlation = mean(.data$edge_correlation, na.rm = TRUE),
      q025_edge_correlation = .InterconectaR_safe_quantile(.data$edge_correlation, 0.025),
      q975_edge_correlation = .InterconectaR_safe_quantile(.data$edge_correlation, 0.975),
      median_edge_correlation_true_edges = stats::median(.data$edge_correlation_true_edges, na.rm = TRUE),
      median_mae_true_edges = stats::median(.data$mae_true_edges, na.rm = TRUE),
      median_sensitivity = stats::median(.data$sensitivity, na.rm = TRUE),
      median_specificity = stats::median(.data$specificity, na.rm = TRUE),
      median_precision = stats::median(.data$precision, na.rm = TRUE),
      median_fdr = stats::median(.data$fdr, na.rm = TRUE),
      median_mae_global = stats::median(.data$mae_global, na.rm = TRUE),
      median_TEFI = stats::median(.data$TEFI, na.rm = TRUE),
      .groups = "drop"
    )

  power_table <- .InterconectaR_summarize_power(raw_results, targets)
  summary <- dplyr::left_join(summary, power_table, by = "Sample_Size")

  # Unified power-based decision: smallest grid n where Pr(meets_all) >= power_target.
  # meets_all already incorporates targets, convergence, and dimension recovery
  # (per fix A2), so this single rule replaces the legacy median-based criterion.
  power_passes <- summary$power_combined >= power_target
  power_passes[is.na(power_passes)] <- FALSE
  summary$meets_criteria <- power_passes

  recommended_n_grid <- if (any(power_passes)) {
    min(summary$Sample_Size[power_passes], na.rm = TRUE)
  } else {
    NA_real_
  }

  # Legacy median-based recommendation: kept for reporting/diagnostic only.
  # Reviewers asked us to unify the decision criterion (fix A1/A3); this column
  # remains so users can compare power-based vs median-based agreement.
  median_passes <- .InterconectaR_evaluate_sample_size_criteria(summary, criteria)
  summary$meets_criteria_median <- median_passes
  recommended_n_grid_median <- if (any(median_passes, na.rm = TRUE)) {
    min(summary$Sample_Size[median_passes], na.rm = TRUE)
  } else {
    NA_real_
  }

  fine_grid <- seq(min(sample_sizes), max(sample_sizes), by = 1L)
  fit <- .InterconectaR_fit_monotone(summary$Sample_Size, summary$power_combined)
  interpolated_curve <- .InterconectaR_interpolate_power(fit, fine_grid)
  recommended_n_interpolated <- .InterconectaR_find_n_star(fit, power_target, fine_grid)
  # A curve cannot be interpolated from a single sample size
  if (length(unique(sample_sizes)) < 2) {
    recommended_n_interpolated <- recommended_n_grid
  }

  recommended_n_ci   <- NULL
  bootstrap_n_star   <- NULL
  bootstrap_power_ci <- NULL
  if (recommendation_method == "interpolated") {
    if (isTRUE(verbose)) {
      message("Bootstrapping recommended n with ", boots, " resamples...")
    }
    # The replications may have run in parallel workers, which do not advance
    # the RNG of this session: seed the bootstrap here so the CI is
    # reproducible with parallel = TRUE as well
    if (!is.null(seed)) set.seed(seed + 2L)
    boot_out <- .InterconectaR_bootstrap_n_star(
      raw_results  = raw_results,
      sample_sizes = sample_sizes,
      target_power = power_target,
      fine_grid    = fine_grid,
      boots        = boots
    )
    bootstrap_n_star <- boot_out$n_star
    # Resamples that never reach the target are censored above the grid, not
    # missing: dropping them would understate the upper bound of the CI
    censored <- !is.finite(bootstrap_n_star)
    recommended_n_ci <- if (sum(!censored) >= 2) {
      boot_for_ci <- bootstrap_n_star
      boot_for_ci[censored] <- Inf
      ci <- stats::quantile(boot_for_ci, probs = c(0.025, 0.5, 0.975),
                            names = TRUE, type = 1)
      if (any(censored) && isTRUE(verbose)) {
        message(sum(censored), " of ", boots, " bootstrap resamples did not reach ",
                "the power target within the grid; the upper CI bound is reported ",
                "as Inf when they exceed 2.5%. Extend 'sample_sizes' upward.")
      }
      ci
    } else {
      c(`2.5%` = NA_real_, `50%` = NA_real_, `97.5%` = NA_real_)
    }
    # 95% bootstrap band of the power curve over the fine grid.
    pc_q <- apply(boot_out$power_curve_matrix, 2,
                  stats::quantile, probs = c(0.025, 0.5, 0.975),
                  na.rm = TRUE, names = FALSE)
    bootstrap_power_ci <- data.frame(
      x      = fine_grid,
      q025   = pc_q[1, ],
      median = pc_q[2, ],
      q975   = pc_q[3, ]
    )
  }

  recommended_n <- if (recommendation_method == "interpolated") {
    recommended_n_interpolated
  } else {
    recommended_n_grid
  }

  # When the smallest candidate already reaches the target, the true minimum
  # may lie below the grid: the curve is extrapolated flat to the left
  if (is.finite(recommended_n) && recommended_n <= min(sample_sizes)) {
    warning("The smallest candidate sample size (", min(sample_sizes),
            ") already reaches the power target, so the minimum required n may ",
            "be lower. Add smaller values to 'sample_sizes' to locate it.",
            call. = FALSE)
  }

  validation_result <- NULL
  if (validation_n_rep > 0L && is.finite(recommended_n)) {
    validation_n <- as.integer(round(recommended_n))
    if (isTRUE(verbose)) {
      message("Running validation at n = ", validation_n,
              " with ", validation_n_rep, " replications...")
    }
    jobs_val <- data.frame(
      sample_size = rep(validation_n, validation_n_rep),
      replication = seq_len(validation_n_rep),
      stringsAsFactors = FALSE
    )
    val_list <- run_mc(jobs_val, seed_offset = 1L)
    validation_raw <- dplyr::bind_rows(val_list)
    validation_raw <- .InterconectaR_compute_per_replication_meets(
      validation_raw, targets,
      require_convergence        = require_convergence,
      require_correct_dimensions = require_correct_dimensions,
      combination                = combination,
      primary_metric             = primary_metric
    )
    validation_result <- list(
      n = validation_n,
      n_rep = validation_n_rep,
      power_combined = mean(validation_raw$meets_all, na.rm = TRUE),
      convergence_rate = mean(validation_raw$converged, na.rm = TRUE),
      p_dimensions = mean(ifelse(is.na(validation_raw$dimensions_correct),
                                 FALSE, validation_raw$dimensions_correct)),
      median_ari = stats::median(validation_raw$ari, na.rm = TRUE),
      median_edge_correlation = stats::median(validation_raw$edge_correlation, na.rm = TRUE),
      median_edge_correlation_true_edges = stats::median(
        validation_raw$edge_correlation_true_edges, na.rm = TRUE),
      median_mae_true_edges = stats::median(validation_raw$mae_true_edges, na.rm = TRUE),
      raw = validation_raw
    )
  }

  out <- list(
    recommended_n = recommended_n,
    recommended_n_grid = recommended_n_grid,
    recommended_n_grid_median = recommended_n_grid_median,
    recommended_n_interpolated = recommended_n_interpolated,
    recommended_n_ci = recommended_n_ci,
    bootstrap_n_star = bootstrap_n_star,
    bootstrap_power_ci = bootstrap_power_ci,
    interpolation = list(
      x = fine_grid,
      y = interpolated_curve,
      target = power_target,
      primary_metric = "power_combined"
    ),
    validation = validation_result,
    summary = summary,
    raw = raw_results,
    criteria = criteria,
    targets = targets,
    population_correlation = sigma,
    population_network = true_network,
    expected_communities = expected_wc,
    structure = list(
      api             = api_mode,
      n_items         = ncol(population_network),
      n_communities   = length(unique(expected_wc)),
      community_sizes = as.integer(table(factor(expected_wc, levels = unique(expected_wc)))),
      community_names = unique(expected_wc),
      within_edge     = if (api_mode == "simple") within_edge else NA_real_,
      n_bridges       = if (api_mode == "simple" && !is.null(bridges)) nrow(as.data.frame(bridges)) else 0L
    ),
    sensitivity = NULL,
    plots = NULL,
    output_dir = output_dir,
    settings = list(
      sample_sizes = sample_sizes,
      n_rep = n_rep,
      network_type = network_type,
      data_type = data_type,
      categories = categories,
      corr = corr,
      model = model,
      algorithm = algorithm,
      uni.method = uni.method,
      power_target = power_target,
      recommendation_method = recommendation_method,
      boots = boots,
      true_edge_threshold = true_edge_threshold,
      estimated_edge_threshold = estimated_edge_threshold,
      edge_threshold_method = edge_threshold_method,
      combination = combination,
      primary_metric = primary_metric,
      parallel = parallel,
      n_workers = if (isTRUE(parallel)) n_workers else NULL,
      seed = seed
    )
  )
  class(out) <- c("sample_size_EGA_montecarlo", class(out))

  if (isTRUE(sensitivity)) {
    out$sensitivity <- .InterconectaR_run_sensitivity(
      population_network = population_network,
      communities        = communities,
      sample_sizes       = sample_sizes,
      n_rep              = max(50L, as.integer(round(n_rep / 2))),
      network_type       = network_type,
      data_type          = data_type,
      power_target       = power_target,
      parallel           = parallel,
      n_workers          = n_workers,
      seed               = if (is.null(seed)) NULL else seed + 1L,
      ctrl               = ctrl,
      verbose            = verbose,
      ega_extra_args     = ega_extra_args,
      targets            = targets,
      baseline_n         = recommended_n_interpolated
    )
  }

  if (isTRUE(plots)) {
    out$plots <- .InterconectaR_make_plots(out)
  }

  if (!is.null(output_dir)) {
    .InterconectaR_persist_output(out, output_dir, verbose = verbose)
  }

  out
}

#' Default control parameters for \code{sample_size_EGA_montecarlo()}
#'
#' Returns a fully populated list of advanced defaults. Pass a partial list
#' through the \code{control} argument of \code{sample_size_EGA_montecarlo()}
#' to override only the entries you care about.
#'
#' Defaults are chosen from the EGA / network-psychometrics literature
#' (Christensen et al., 2024; Golino et al., 2020). Each entry supplied in
#' \code{control} replaces the default as a whole; for instance,
#' \code{control = list(targets = list(ari = 0.80))} leaves ARI as the only
#' target.
#'
#' @param control Optional named list with overrides.
#' @return A named list with all advanced parameters resolved.
#' @export
#' @examples
#' # Defaults
#' str(sample_size_EGA_control(), max.level = 1)
#'
#' # Use only community recovery as the decisional target
#' sample_size_EGA_control(list(targets = list(ari = 0.80)))$targets
sample_size_EGA_control <- function(control = list()) {
  defaults <- list(
    categories               = 5L,
    thresholds               = NULL,
    corr                     = "cor_auto",
    model                    = "glasso",
    algorithm                = "leiden",
    uni.method               = "louvain",
    criteria                 = list(
      convergence_rate = 0.95,
      p_dimensions     = 0.90,
      ari              = 0.80,
      edge_correlation = 0.70
    ),
    targets                  = list(ari = 0.80, edge_correlation = 0.70),
    recommendation_method    = "interpolated",
    boots                    = 1000L,
    require_convergence      = TRUE,
    require_correct_dimensions = TRUE,
    combination              = "and",
    primary_metric           = NULL,
    true_edge_threshold      = 0.05,
    estimated_edge_threshold = 0.05,
    edge_threshold_method    = "absolute"
  )

  if (length(control) == 0) return(defaults)

  if (!is.list(control) || is.null(names(control)) || any(names(control) == "")) {
    stop("'control' must be a fully named list.")
  }
  unknown <- setdiff(names(control), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown 'control' entries: ", paste(unknown, collapse = ", "),
         ". Allowed: ", paste(names(defaults), collapse = ", "))
  }
  # Each supplied entry replaces the default as a whole. modifyList() would
  # merge nested lists, so control = list(targets = list(ari = .80)) would
  # silently keep the default edge_correlation target as well.
  resolved <- defaults
  resolved[names(control)] <- control

  if (!resolved$recommendation_method %in% c("grid", "interpolated")) {
    stop("control$recommendation_method must be 'grid' or 'interpolated'.")
  }
  if (!resolved$combination %in% c("and", "primary")) {
    stop("control$combination must be 'and' or 'primary'.")
  }
  if (!resolved$edge_threshold_method %in% c("absolute", "match_true")) {
    stop("control$edge_threshold_method must be 'absolute' or 'match_true'.")
  }

  resolved
}

# Internal: run the council-flagged sensitivity sweep with reduced n_rep so
# the cost stays bounded. Keeps the public function small while letting users
# trigger the full robustness report with a single flag.
.InterconectaR_run_sensitivity <- function(population_network, communities,
                                           sample_sizes, n_rep, network_type,
                                           data_type, power_target,
                                           parallel, n_workers, seed,
                                           ctrl, verbose, ega_extra_args,
                                           targets, baseline_n) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  if (isTRUE(verbose)) {
    message("Running sensitivity sweep (threshold x edge_method x combination)...")
  }

  call_internal <- function(ctrl_overrides) {
    # Replace entries as a whole: modifyList() would not replace an unnamed
    # list of per-item thresholds
    new_ctrl <- ctrl
    new_ctrl[names(ctrl_overrides)] <- ctrl_overrides
    # Cheap bootstrap inside the sweep: 200 resamples are enough for a
    # cross-check; 1000 was making each sub-call ~5x slower than necessary.
    new_ctrl$boots <- 200L
    args <- c(
      list(
        population_network = population_network,
        communities        = communities,
        sample_sizes       = sample_sizes,
        n_rep              = n_rep,
        network_type       = network_type,
        data_type          = data_type,
        power_target       = power_target,
        validation_n_rep   = 0L,
        sensitivity        = FALSE,
        plots              = FALSE,
        output_dir         = NULL,
        parallel           = parallel,
        n_workers          = n_workers,
        seed               = seed,
        verbose            = FALSE,
        control            = new_ctrl
      ),
      ega_extra_args
    )
    suppressWarnings(do.call(sample_size_EGA_montecarlo, args))
  }

  # The edge thresholds only enter the binary and true-edge metrics. When none
  # of the targets uses them, the threshold sweeps cannot move n and would only
  # show Monte Carlo noise, so they are skipped.
  threshold_metrics <- c("sensitivity", "specificity", "precision", "fdr",
                         "edge_correlation_true_edges", "mae_true_edges")
  uses_thresholds <- length(intersect(names(targets), threshold_metrics)) > 0

  df_threshold <- NULL
  df_method <- NULL
  if (uses_thresholds) {
    threshold_grid <- c(0.01, 0.05, 0.10)
    threshold_runs <- lapply(threshold_grid, function(thr) {
      call_internal(list(
        true_edge_threshold      = thr,
        estimated_edge_threshold = thr,
        edge_threshold_method    = "absolute"
      ))
    })
    df_threshold <- data.frame(
      true_edge_threshold        = threshold_grid,
      recommended_n_grid         = vapply(threshold_runs,
                                          function(r) r$recommended_n_grid, numeric(1)),
      recommended_n_interpolated = vapply(threshold_runs,
                                          function(r) r$recommended_n_interpolated, numeric(1))
    )

    other_method <- if (ctrl$edge_threshold_method == "match_true") "absolute" else "match_true"
    res_method <- call_internal(list(edge_threshold_method = other_method))
    df_method <- data.frame(
      edge_threshold_method      = c(ctrl$edge_threshold_method, other_method),
      recommended_n_interpolated = c(baseline_n, res_method$recommended_n_interpolated)
    )
  } else if (isTRUE(verbose)) {
    message("Skipping the edge-threshold sweeps: none of the targets (",
            paste(names(targets), collapse = ", "),
            ") depends on the edge thresholds.")
  }

  primary_first <- if (length(targets) == 0) "ari" else names(targets)[1]
  if (ctrl$combination == "primary") {
    res_combination <- call_internal(list(combination = "and"))
    alt_label <- "and"
    base_label <- paste0("primary (", ctrl$primary_metric %||% primary_first, ")")
  } else {
    res_combination <- call_internal(list(
      combination    = "primary",
      primary_metric = primary_first
    ))
    alt_label <- paste0("primary (", primary_first, ")")
    base_label <- "and"
  }
  df_combination <- data.frame(
    combination                = c(base_label, alt_label),
    recommended_n_interpolated = c(baseline_n, res_combination$recommended_n_interpolated)
  )

  # Council fix (C): asymmetric Likert thresholds (floor/ceiling) typically
  # raise n requirements vs the equiprobable default. Run a "floor" case so
  # users see the impact of realistic ordinal patterns. Only meaningful for
  # ordinal data; falls back to a built-in floor builder if the exported
  # make_likert_thresholds() is unavailable for any reason.
  df_likert <- NULL
  if (data_type == "ordinal") {
    n_items_in <- ncol(population_network)
    floor_thr <- tryCatch(
      make_likert_thresholds(n_items_in, ctrl$categories, "floor"),
      error = function(e) .InterconectaR_floor_thresholds_fallback(
        n_items_in, ctrl$categories
      )
    )
    if (!is.null(floor_thr)) {
      res_floor <- call_internal(list(thresholds = floor_thr))
      base_thr_label <- if (is.null(ctrl$thresholds)) "equiprobable (default)" else "user-defined"
      df_likert <- data.frame(
        thresholds                 = c(base_thr_label, "floor"),
        recommended_n_interpolated = c(baseline_n, res_floor$recommended_n_interpolated)
      )
    }
  }

  list(
    threshold   = df_threshold,
    method      = df_method,
    combination = df_combination,
    likert      = df_likert
  )
}

# Internal: build the standard plot panel. Wrapped here so the public function
# can stay declarative (plots = TRUE).
.InterconectaR_make_plots <- function(result) {
  has_pkg <- requireNamespace("ggplot2", quietly = TRUE)
  if (!has_pkg) {
    warning("'ggplot2' not installed; skipping plot generation.", call. = FALSE)
    return(NULL)
  }

  out <- list()
  out$power_curve_ci <- tryCatch(
    plot_power_curve_ci(result, show_validation = TRUE, show_observed = TRUE),
    error = function(e) NULL
  )
  out$failure_decomposition <- tryCatch(
    plot_failure_decomposition(result),
    error = function(e) NULL
  )
  out$metrics <- tryCatch(
    plot_sample_size_EGA_montecarlo(result, show_uncertainty = TRUE),
    error = function(e) NULL
  )
  out$bootstrap_n_star <- .InterconectaR_plot_bootstrap_hist(result)
  out
}

.InterconectaR_plot_bootstrap_hist <- function(result) {
  if (is.null(result$bootstrap_n_star) || is.null(result$recommended_n_ci)) {
    return(NULL)
  }
  ns <- result$bootstrap_n_star
  ns <- ns[is.finite(ns)]
  if (length(ns) < 2) return(NULL)

  ci <- result$recommended_n_ci
  df <- data.frame(n_star = ns)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$n_star)) +
    ggplot2::geom_histogram(bins = 30, fill = "#7570B3",
                            color = "white", alpha = 0.75) +
    ggplot2::geom_vline(xintercept = ci["2.5%"],  linetype = "dotted", color = "grey20") +
    ggplot2::geom_vline(xintercept = ci["50%"],   linetype = "dashed", color = "#111111") +
    ggplot2::geom_vline(xintercept = ci["97.5%"], linetype = "dotted", color = "grey20") +
    ggplot2::labs(
      title    = "Bootstrap distribution of N*",
      subtitle = sprintf("95%% CI = [%.0f, %.0f]  |  median = %.0f",
                         ci["2.5%"], ci["97.5%"], ci["50%"]),
      x        = "N* (smallest n reaching power_target)",
      y        = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))
}

# Internal: write everything (RDS, summary CSV, sensitivity CSVs, plot PNGs)
# to output_dir. Creates the directory if needed. No-op when output_dir = NULL.
.InterconectaR_persist_output <- function(result, output_dir, verbose) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (isTRUE(verbose)) {
    message("Writing results to ", normalizePath(output_dir, mustWork = FALSE))
  }

  saveRDS(result, file.path(output_dir, "result.rds"))
  utils::write.csv(result$summary,
                   file.path(output_dir, "summary.csv"),
                   row.names = FALSE)

  if (!is.null(result$sensitivity)) {
    for (key in names(result$sensitivity)) {
      utils::write.csv(result$sensitivity[[key]],
                       file.path(output_dir, paste0("sensitivity_", key, ".csv")),
                       row.names = FALSE)
    }
  }

  if (!is.null(result$plots) && requireNamespace("ggplot2", quietly = TRUE)) {
    for (nm in names(result$plots)) {
      gg <- result$plots[[nm]]
      if (is.null(gg)) next
      ggplot2::ggsave(
        filename = file.path(output_dir, paste0(nm, ".png")),
        plot     = gg,
        width    = 9.5, height = 6, dpi = 300
      )
    }
  }
  invisible(NULL)
}

# Internal: build a partial-correlation population network and community
# assignment from the structural parameters (factor_sizes, within_edge,
# bridges, factor_names). Powers the simple API of sample_size_EGA_montecarlo().
.InterconectaR_build_partial_network <- function(community_sizes, within_edge,
                                                 bridges, community_names) {
  if (!is.numeric(community_sizes) || length(community_sizes) == 0 ||
      any(community_sizes < 2) ||
      any(abs(community_sizes - round(community_sizes)) > .Machine$double.eps^0.5)) {
    stop("'community_sizes' must be a vector of integers >= 2.")
  }
  if (!is.numeric(within_edge) || length(within_edge) != 1 ||
      abs(within_edge) >= 1) {
    stop("'within_edge' must be a single numeric in (-1, 1).")
  }
  community_sizes <- as.integer(community_sizes)
  k <- length(community_sizes)
  if (is.null(community_names)) community_names <- paste0("F", seq_len(k))
  if (length(community_names) != k) {
    stop("'community_names' must have one element per community.")
  }

  n_items    <- sum(community_sizes)
  item_names <- paste0("i", seq_len(n_items))
  net <- matrix(0, nrow = n_items, ncol = n_items,
                dimnames = list(item_names, item_names))

  starts <- c(0L, cumsum(community_sizes)[-k])
  communities <- vector("list", k)
  names(communities) <- community_names
  for (f in seq_len(k)) {
    idx <- (starts[f] + 1L):(starts[f] + community_sizes[f])
    net[idx, idx] <- within_edge
    communities[[f]] <- idx
  }
  diag(net) <- 0

  if (!is.null(bridges)) {
    bridges <- as.data.frame(bridges)
    needed <- c("i", "j", "weight")
    if (!all(needed %in% names(bridges))) {
      stop("'bridges' must have columns: i, j, weight.")
    }
    for (b in seq_len(nrow(bridges))) {
      i <- bridges$i[b]; j <- bridges$j[b]; w <- bridges$weight[b]
      if (i < 1 || i > n_items || j < 1 || j > n_items) {
        stop("'bridges' references item index out of range (1..", n_items, ").")
      }
      net[i, j] <- net[j, i] <- w
    }
  }

  list(network = net, communities = communities)
}

.InterconectaR_population_to_correlation <- function(population_network, network_type) {
  population_network <- as.matrix(population_network)
  population_network <- (population_network + t(population_network)) / 2

  if (network_type == "correlation") {
    diag(population_network) <- 1
    sigma <- population_network
  } else {
    diag(population_network) <- 0
    precision <- diag(nrow(population_network))
    precision[upper.tri(precision)] <- -population_network[upper.tri(population_network)]
    precision[lower.tri(precision)] <- t(precision)[lower.tri(precision)]
    min_eig <- min(eigen(precision, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eig <= 1e-08) {
      # A block of m items that all share the weight w has eigenvalue
      # 1 - w * (m - 1), so the partial weights must stay below 1 / (m - 1)
      stop("The partial network implies a precision matrix (K = I - P) that is ",
           "not positive definite (smallest eigenvalue = ", signif(min_eig, 3),
           "). Lower the edge weights: a community of m items that all share ",
           "the weight w needs w < 1 / (m - 1); for example, 12 items need ",
           "w < ", round(1 / 11, 3), ".", call. = FALSE)
    }
    cov_matrix <- tryCatch(solve(precision), error = function(e) {
      stop("Implied precision matrix is singular: cannot derive a correlation matrix from the partial network.")
    })
    sigma <- stats::cov2cor(cov_matrix)
  }

  sigma <- (sigma + t(sigma)) / 2
  eig <- eigen(sigma, symmetric = TRUE, only.values = TRUE)$values
  if (min(eig) <= 1e-08) {
    stop("The implied population correlation matrix is not positive definite.")
  }

  sigma
}

.InterconectaR_correlation_to_partial <- function(sigma) {
  K <- tryCatch(solve(sigma), error = function(e) {
    stop("Population correlation matrix is not invertible; cannot derive implied partial correlations.")
  })
  d <- sqrt(diag(K))
  pcor <- -K / outer(d, d)
  diag(pcor) <- 0
  (pcor + t(pcor)) / 2
}

.InterconectaR_simulate_from_sigma <- function(n, sigma, data_type, categories, thresholds) {
  p <- ncol(sigma)
  z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  simulated <- z %*% chol(sigma)
  colnames(simulated) <- colnames(sigma)

  if (data_type == "ordinal") {
    # Fix B6a: resolve thresholds into a per-item list so each item can have
    # its own (asymmetric) cutoffs (floor/ceiling effects, mixed scales).
    item_thresholds <- .InterconectaR_resolve_thresholds(thresholds, p, categories)

    simulated <- vapply(seq_len(p), function(j) {
      tau <- item_thresholds[[j]]
      as.integer(cut(simulated[, j], breaks = c(-Inf, tau, Inf), labels = FALSE))
    }, integer(n))
    # Fix B6b: explicitly convert to ordered factors so EGAnet/cor_auto/lavCor
    # reliably trigger polychoric correlation regardless of the nLevels heuristic.
    levels_seq <- seq_len(categories)
    simulated <- as.data.frame(
      lapply(as.data.frame(simulated), function(x) ordered(x, levels = levels_seq))
    )
    colnames(simulated) <- colnames(sigma)
  } else {
    simulated <- as.data.frame(simulated)
  }

  simulated
}

# Resolve a thresholds argument into a list of length p (one numeric vector
# of length categories - 1 per item). Accepts:
#   NULL      -> equiprobable categories (qnorm seq/k)
#   numeric   -> same vector for all items (length must be categories - 1)
#   list      -> per-item vectors (length p)
#   matrix    -> per-item columns (categories - 1) x p
# Internal fallback if make_likert_thresholds() is not reachable: builds a
# "floor" pattern (low endorsement, mass on the lowest categories) by shifting
# the equiprobable thresholds upward. Mirrors the public function's behaviour
# closely enough for the sensitivity sweep to remain informative.
.InterconectaR_floor_thresholds_fallback <- function(p, categories) {
  base <- stats::qnorm(seq_len(categories - 1L) / categories)
  shifted <- base + 0.75
  replicate(p, shifted, simplify = FALSE)
}

.InterconectaR_resolve_thresholds <- function(thresholds, p, categories) {
  expected_len <- categories - 1L

  if (is.null(thresholds)) {
    tau <- stats::qnorm(seq_len(expected_len) / categories)
    return(replicate(p, tau, simplify = FALSE))
  }
  if (is.list(thresholds)) {
    if (length(thresholds) != p) {
      stop("'thresholds' as list must have one vector per item (length = ",
           p, ").")
    }
    bad <- vapply(thresholds, function(x) {
      !is.numeric(x) || length(x) != expected_len
    }, logical(1))
    if (any(bad)) {
      stop("Each element of 'thresholds' must be numeric of length = categories - 1 (",
           expected_len, ").")
    }
    return(thresholds)
  }
  if (is.matrix(thresholds)) {
    if (nrow(thresholds) != expected_len || ncol(thresholds) != p) {
      stop("'thresholds' as matrix must have dimensions (categories-1) x p (",
           expected_len, " x ", p, ").")
    }
    return(lapply(seq_len(p), function(j) thresholds[, j]))
  }
  if (is.numeric(thresholds)) {
    if (length(thresholds) != expected_len) {
      stop("'thresholds' as vector must have length = categories - 1 (",
           expected_len, ").")
    }
    return(replicate(p, thresholds, simplify = FALSE))
  }
  stop("'thresholds' must be NULL, a numeric vector, a list, or a matrix.")
}

.InterconectaR_communities_to_vector <- function(communities, item_names) {
  if (is.list(communities)) {
    if (is.null(names(communities)) || any(!nzchar(names(communities)))) {
      names(communities) <- paste0("F", seq_along(communities))
      warning("'communities' list had no (or empty) names; auto-assigning ",
              "F1, F2, ... For reproducible reporting prefer naming the list ",
              "yourself.", call. = FALSE)
    }
    wc <- rep(NA_character_, length(item_names))
    names(wc) <- item_names
    for (community_name in names(communities)) {
      members <- communities[[community_name]]
      if (is.numeric(members)) {
        members <- item_names[members]
      }
      wc[members] <- community_name
    }
    if (any(is.na(wc))) {
      stop("'communities' does not assign all items to a dimension.")
    }
    return(wc)
  }

  if (length(communities) != length(item_names)) {
    stop("'communities' must have one value per item.")
  }

  wc <- as.character(communities)
  names(wc) <- item_names
  wc
}

.InterconectaR_network_metrics <- function(
    true_network,
    estimated_network,
    true_edge_threshold,
    estimated_edge_threshold
) {
  true_network <- as.matrix(true_network)
  estimated_network <- as.matrix(estimated_network)

  # EGAnet sometimes returns a network whose dimnames don't perfectly match
  # the input (e.g. ordered factor rename). Reindex defensively: align on the
  # intersection of names; if dimensions still mismatch, abort cleanly so the
  # outer try/catch can record the failure as a non-converging replicate.
  common <- intersect(rownames(true_network), rownames(estimated_network))
  if (length(common) != nrow(true_network)) {
    stop("estimated_network rownames do not match true_network rownames ",
         "(", length(common), "/", nrow(true_network), " match).")
  }
  estimated_network <- estimated_network[common, common, drop = FALSE]
  true_network      <- true_network[common, common, drop = FALSE]

  diag(true_network) <- 0
  diag(estimated_network) <- 0

  selector <- upper.tri(true_network)
  true_weights <- true_network[selector]
  estimated_weights <- estimated_network[selector]

  true_edges <- abs(true_weights) > true_edge_threshold
  estimated_edges <- abs(estimated_weights) > estimated_edge_threshold

  true_positive <- sum(estimated_edges & true_edges, na.rm = TRUE)
  false_positive <- sum(estimated_edges & !true_edges, na.rm = TRUE)
  true_negative <- sum(!estimated_edges & !true_edges, na.rm = TRUE)
  false_negative <- sum(!estimated_edges & true_edges, na.rm = TRUE)

  # Council fix (B): edge_correlation over ALL upper-triangle entries can be
  # inflated by the mass of zeros when the true network is sparse ("zero
  # matches zero"). We complement it with metrics computed ONLY over true
  # non-zero edges so users can detect this kind of inflation.
  true_only_idx <- true_edges & is.finite(true_weights) & is.finite(estimated_weights)
  edge_correlation_true_edges <- if (sum(true_only_idx) >= 2) {
    .InterconectaR_safe_cor(true_weights[true_only_idx],
                            estimated_weights[true_only_idx])
  } else {
    NA_real_
  }
  mae_true_edges <- if (sum(true_only_idx) >= 1) {
    mean(abs(true_weights[true_only_idx] - estimated_weights[true_only_idx]),
         na.rm = TRUE)
  } else {
    NA_real_
  }

  list(
    sensitivity = .InterconectaR_safe_ratio(true_positive, true_positive + false_negative),
    specificity = .InterconectaR_safe_ratio(true_negative, true_negative + false_positive),
    precision = .InterconectaR_safe_ratio(true_positive, true_positive + false_positive),
    fdr = .InterconectaR_safe_ratio(false_positive, true_positive + false_positive),
    edge_correlation = .InterconectaR_safe_cor(true_weights, estimated_weights),
    edge_correlation_true_edges = edge_correlation_true_edges,
    mae_true_edges = mae_true_edges,
    # Council fix: this is mean absolute error over ALL upper-triangle entries,
    # not signed bias. Renamed mae_global to avoid the misnomer.
    mae_global = mean(abs(true_weights - estimated_weights), na.rm = TRUE)
  )
}

.InterconectaR_safe_ratio <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  numerator / denominator
}

.InterconectaR_safe_cor <- function(x, y) {
  valid <- is.finite(x) & is.finite(y)
  if (sum(valid) < 2 || stats::sd(x[valid]) == 0 || stats::sd(y[valid]) == 0) {
    return(NA_real_)
  }
  stats::cor(x[valid], y[valid])
}

.InterconectaR_safe_quantile <- function(x, probs) {
  valid <- x[is.finite(x)]
  if (length(valid) < 2) {
    return(NA_real_)
  }
  unname(stats::quantile(valid, probs = probs, na.rm = TRUE, names = FALSE))
}

.InterconectaR_adjusted_rand_index <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)
  # EGA leaves isolated items without a community (NA). Dropping them would
  # reward an incomplete solution with ARI = 1, so each one counts as its own
  # singleton community instead.
  unassigned <- is.na(y)
  y[unassigned] <- paste0(".unassigned_", which(unassigned))
  valid <- !is.na(x)
  x <- x[valid]
  y <- y[valid]

  if (length(x) < 2) {
    return(NA_real_)
  }

  tab <- table(x, y)
  choose2 <- function(v) v * (v - 1) / 2

  sum_comb <- sum(choose2(tab))
  row_comb <- sum(choose2(rowSums(tab)))
  col_comb <- sum(choose2(colSums(tab)))
  total_comb <- choose2(sum(tab))

  expected_index <- row_comb * col_comb / total_comb
  max_index <- (row_comb + col_comb) / 2

  if (max_index == expected_index) {
    return(NA_real_)
  }

  (sum_comb - expected_index) / (max_index - expected_index)
}

.InterconectaR_evaluate_sample_size_criteria <- function(summary, criteria) {
  pass <- rep(TRUE, nrow(summary))

  if (!is.null(criteria$convergence_rate)) {
    pass <- pass & summary$convergence_rate >= criteria$convergence_rate
  }
  if (!is.null(criteria$p_dimensions)) {
    pass <- pass & summary$p_dimensions >= criteria$p_dimensions
  }
  if (!is.null(criteria$ari)) {
    pass <- pass & summary$median_ari >= criteria$ari
  }
  if (!is.null(criteria$edge_correlation)) {
    pass <- pass & summary$median_edge_correlation >= criteria$edge_correlation
  }
  if (!is.null(criteria$edge_correlation_true_edges)) {
    pass <- pass & summary$median_edge_correlation_true_edges >=
      criteria$edge_correlation_true_edges
  }
  if (!is.null(criteria$mae_true_edges)) {
    pass <- pass & summary$median_mae_true_edges <= criteria$mae_true_edges
  }
  if (!is.null(criteria$sensitivity)) {
    pass <- pass & summary$median_sensitivity >= criteria$sensitivity
  }
  if (!is.null(criteria$specificity)) {
    pass <- pass & summary$median_specificity >= criteria$specificity
  }
  if (!is.null(criteria$precision)) {
    pass <- pass & summary$median_precision >= criteria$precision
  }
  if (!is.null(criteria$fdr)) {
    pass <- pass & summary$median_fdr <= criteria$fdr
  }

  pass[is.na(pass)] <- FALSE
  pass
}

.InterconectaR_metric_directions <- function() {
  c(
    ari                          = ">=",
    edge_correlation             = ">=",
    edge_correlation_true_edges  = ">=",
    mae_true_edges               = "<=",
    sensitivity                  = ">=",
    specificity                  = ">=",
    precision                    = ">=",
    fdr                          = "<="
  )
}

# Add per-replication "meets_<metric>" boolean columns and a "meets_all" flag
# given a list of per-replication thresholds (targets).
#
# meets_all combines: target metrics AND (optionally) converged AND
# (optionally) dimensions_correct. A replication that did not converge or did
# not recover the expected number of dimensions is treated as failing.
#
# Fix B7: 'combination' controls how target metrics are AND-combined.
#   "and"     -> all target metrics must be met (default, conservative)
#   "primary" -> only meets_<primary_metric> participates; the others are
#                still computed for diagnostic but do not gate meets_all.
.InterconectaR_compute_per_replication_meets <- function(raw_results, targets,
                                                         require_convergence = TRUE,
                                                         require_correct_dimensions = TRUE,
                                                         combination = "and",
                                                         primary_metric = NULL) {
  directions <- .InterconectaR_metric_directions()
  meets_cols <- character(0)

  for (metric in names(targets)) {
    direction <- directions[metric]
    if (is.na(direction) || !metric %in% names(raw_results)) next
    target_val <- targets[[metric]]
    col_name <- paste0("meets_", metric)
    values <- raw_results[[metric]]
    raw_results[[col_name]] <- if (direction == ">=") {
      !is.na(values) & values >= target_val
    } else {
      !is.na(values) & values <= target_val
    }
    meets_cols <- c(meets_cols, col_name)
  }

  # Subset of meets_cols that drive meets_all (depends on combination).
  driving_cols <- if (combination == "primary") {
    primary_col <- paste0("meets_", primary_metric)
    intersect(primary_col, meets_cols)
  } else {
    meets_cols
  }

  base <- if (length(driving_cols) > 0) {
    rowSums(as.data.frame(raw_results[, driving_cols, drop = FALSE])) == length(driving_cols)
  } else {
    rep(TRUE, nrow(raw_results))
  }

  if (isTRUE(require_convergence) && "converged" %in% names(raw_results)) {
    converged_flag <- ifelse(is.na(raw_results$converged), FALSE,
                             as.logical(raw_results$converged))
    base <- base & converged_flag
  }
  if (isTRUE(require_correct_dimensions) && "dimensions_correct" %in% names(raw_results)) {
    dim_flag <- ifelse(is.na(raw_results$dimensions_correct), FALSE,
                       as.logical(raw_results$dimensions_correct))
    base <- base & dim_flag
  }

  raw_results$meets_all <- base
  raw_results
}

# Per-sample-size summary of power columns (Pr(meet target)).
.InterconectaR_summarize_power <- function(raw_results, targets) {
  directions <- .InterconectaR_metric_directions()
  meets_cols <- intersect(
    paste0("meets_", names(targets)[names(targets) %in% names(directions)]),
    names(raw_results)
  )
  power_cols <- gsub("^meets_", "power_", meets_cols)

  out <- raw_results %>%
    dplyr::group_by(.data$Sample_Size) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(meets_cols), ~ mean(.x, na.rm = TRUE)),
      power_combined = mean(.data$meets_all, na.rm = TRUE),
      .groups = "drop"
    )

  if (length(meets_cols) > 0) {
    rename_map <- stats::setNames(meets_cols, power_cols)
    out <- dplyr::rename(out, !!!rename_map)
  }

  out
}

# Monotone (PAV) fit of the power curve.
.InterconectaR_fit_monotone <- function(x, y) {
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  valid <- is.finite(y)
  if (sum(valid) < 2) return(NULL)
  iso <- stats::isoreg(x[valid], y[valid])
  list(x = x[valid], yf = iso$yf)
}

.InterconectaR_interpolate_power <- function(fit, fine_grid) {
  if (is.null(fit)) return(rep(NA_real_, length(fine_grid)))
  stats::approx(fit$x, fit$yf, xout = fine_grid, rule = 2)$y
}

# Smallest n on the fine grid where interpolated power crosses the target.
.InterconectaR_find_n_star <- function(fit, target, fine_grid) {
  if (is.null(fit)) return(NA_real_)
  yhat <- .InterconectaR_interpolate_power(fit, fine_grid)
  hits <- which(yhat >= target)
  if (length(hits) == 0) return(NA_real_)
  fine_grid[min(hits)]
}

# Stratified bootstrap of the recommended n: resample replications within each
# sample size, average meets_all (which already incorporates targets, convergence
# and dimension recovery), refit isotonic curve, find n_star and store the
# interpolated curve so we can later draw a 95% bootstrap band.
.InterconectaR_bootstrap_n_star <- function(raw_results, sample_sizes, target_power,
                                            fine_grid, boots) {
  raw_split <- split(raw_results$meets_all, raw_results$Sample_Size)
  n_keys <- as.character(sample_sizes)
  n_grid <- length(fine_grid)

  n_star_dist <- numeric(boots)
  power_curve_matrix <- matrix(NA_real_, nrow = boots, ncol = n_grid)

  for (b in seq_len(boots)) {
    power_b <- numeric(length(sample_sizes))
    for (i in seq_along(sample_sizes)) {
      vec <- raw_split[[n_keys[i]]]
      if (is.null(vec) || length(vec) == 0) {
        power_b[i] <- NA_real_
        next
      }
      idx <- sample.int(length(vec), length(vec), replace = TRUE)
      power_b[i] <- mean(vec[idx], na.rm = TRUE)
    }
    fit_b <- .InterconectaR_fit_monotone(sample_sizes, power_b)
    if (!is.null(fit_b)) {
      power_curve_matrix[b, ] <- .InterconectaR_interpolate_power(fit_b, fine_grid)
    }
    n_star_dist[b] <- .InterconectaR_find_n_star(fit_b, target_power, fine_grid)
  }

  list(
    n_star = n_star_dist,
    power_curve_matrix = power_curve_matrix
  )
}


#' Narrative summary of a sample-size simulation
#'
#' Prints a one-paragraph dynamic summary of the recommended sample size
#' together with the structural assumptions that led to it (number of items,
#' number of dimensions, factor sizes, replications, power target,
#' validation result and sensitivity sweep).
#'
#' @param x A \code{sample_size_EGA_montecarlo} object.
#' @param digits Number of significant digits for non-integer values.
#' @param show_plots Logical; if \code{TRUE} and the object carries plots
#'   (\code{plots = TRUE} at fit time), each plot is rendered after the
#'   narrative. Default \code{FALSE}.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
#' @examples
#' res <- sample_size_EGA_montecarlo(
#'   community_sizes = c(4, 4), within_edge = 0.20,
#'   sample_sizes = c(100, 200, 400), n_rep = 10,
#'   data_type = "continuous", seed = 1, verbose = FALSE,
#'   control = list(boots = 100, corr = "pearson", algorithm = "walktrap")
#' )
#' print(res)
print.sample_size_EGA_montecarlo <- function(x, digits = 3,
                                             show_plots = FALSE, ...) {
  s <- x$structure
  set <- x$settings

  cat("Sample-size simulation for EGA\n")
  cat(strrep("-", 32), "\n", sep = "")

  cat(sprintf(
    "Para %d items en %d comunidades (tamanos: %s), datos %s",
    s$n_items, s$n_communities,
    paste(s$community_sizes, collapse = "-"),
    set$data_type
  ))
  if (!is.na(s$within_edge)) {
    cat(sprintf(", aristas internas = %.2f", s$within_edge))
  }
  if (!is.na(s$n_bridges) && s$n_bridges > 0) {
    cat(sprintf(", %d puente(s) inter-dimension", s$n_bridges))
  }
  cat(".\n")

  rec_method <- if (is.null(set$recommendation_method)) "interpolated" else set$recommendation_method
  cat(sprintf(
    "Monte Carlo: %d replicas en cada uno de los tamanos %s, power_target = %.2f (%s).\n\n",
    set$n_rep,
    paste(set$sample_sizes, collapse = ", "),
    x$interpolation$target,
    rec_method
  ))

  rec <- x$recommended_n
  if (is.finite(rec)) {
    cat(sprintf(">> Se recomienda N = %s datos.\n", format(round(rec))))
  } else {
    cat(">> N recomendado: no alcanzado en la grilla actual ",
        "(extender 'sample_sizes' hacia valores mayores).\n", sep = "")
  }

  if (!is.null(x$recommended_n_ci) &&
      is.finite(x$recommended_n_ci["2.5%"])) {
    n_boot <- length(x$bootstrap_n_star)
    upper <- if (is.finite(x$recommended_n_ci["97.5%"])) {
      sprintf("%.0f", x$recommended_n_ci["97.5%"])
    } else {
      sprintf("> %g", max(set$sample_sizes))
    }
    cat(sprintf("   IC 95%% (bootstrap, %d reps internos): [%.0f, %s]\n",
                n_boot,
                x$recommended_n_ci["2.5%"],
                upper))
  }

  if (!is.null(x$validation)) {
    v <- x$validation
    ok <- v$power_combined >= x$interpolation$target
    cat(sprintf("   Validacion en N = %d con %d reps: power = %.2f (%s target).\n",
                v$n, v$n_rep, v$power_combined,
                if (ok) "cumple" else "no cumple"))
  }

  if (!is.null(x$sensitivity)) {
    cat("\nSensibilidad (cambio de N* segun decisiones arbitrarias):\n")
    sens <- x$sensitivity
    fmt_n <- function(v) {
      out <- ifelse(is.finite(v), format(round(v), trim = TRUE), "NA")
      paste(out, collapse = " / ")
    }
    if (!is.null(sens$threshold)) {
      th <- sens$threshold
      cat(sprintf("  true_edge_threshold %s -> N* %s\n",
                  paste(sprintf("%.2f", th$true_edge_threshold), collapse = " / "),
                  fmt_n(th$recommended_n_interpolated)))
    }
    if (!is.null(sens$method)) {
      cat(sprintf("  edge_threshold_method %s -> N* %s\n",
                  paste(sens$method$edge_threshold_method, collapse = " / "),
                  fmt_n(sens$method$recommended_n_interpolated)))
    }
    if (!is.null(sens$combination)) {
      cat(sprintf("  combination %s -> N* %s\n",
                  paste(sens$combination$combination, collapse = " / "),
                  fmt_n(sens$combination$recommended_n_interpolated)))
    }
    if (!is.null(sens$likert)) {
      cat(sprintf("  thresholds %s -> N* %s\n",
                  paste(sens$likert$thresholds, collapse = " / "),
                  fmt_n(sens$likert$recommended_n_interpolated)))
    }
  }

  if (!is.null(x$plots)) {
    cat("\nPlots disponibles en $plots: ",
        paste(names(x$plots), collapse = ", "), ".\n", sep = "")
    cat("Use show_plots(x) para visualizarlos uno por uno.\n")
  }

  if (!is.null(x$output_dir)) {
    cat(sprintf("\nResultados persistidos en: %s\n", x$output_dir))
  }

  if (isTRUE(show_plots) && !is.null(x$plots)) {
    show_plots(x)
  }

  invisible(x)
}


#' One-line dynamic recommendation message
#'
#' Reports, as a message, a single sentence with the recommended sample size and the structural
#' assumptions that produced it (items, communities, sizes, data type, power
#' target, bootstrap CI when available). Use this when you want the answer
#' fast and don't need the full \code{print()} report.
#'
#' @param x A \code{sample_size_EGA_montecarlo} object.
#' @return Invisibly returns the recommended \code{n}.
#' @export
#' @examples
#' res <- sample_size_EGA_montecarlo(
#'   community_sizes = c(4, 4), within_edge = 0.20,
#'   sample_sizes = c(100, 200, 400), n_rep = 10,
#'   data_type = "continuous", seed = 1, verbose = FALSE,
#'   control = list(boots = 100, corr = "pearson", algorithm = "walktrap")
#' )
#' show_recommendation(res)
show_recommendation <- function(x) {
  if (!inherits(x, "sample_size_EGA_montecarlo")) {
    stop("'x' must be a 'sample_size_EGA_montecarlo' object.")
  }
  s   <- x$structure
  set <- x$settings
  rec <- x$recommended_n

  ci_txt <- ""
  if (!is.null(x$recommended_n_ci) &&
      is.finite(x$recommended_n_ci["2.5%"])) {
    upper <- if (is.finite(x$recommended_n_ci["97.5%"])) {
      sprintf("%.0f", x$recommended_n_ci["97.5%"])
    } else {
      sprintf("> %g", max(set$sample_sizes))
    }
    ci_txt <- sprintf(" (IC 95%%: %.0f-%s)",
                      x$recommended_n_ci["2.5%"], upper)
  }

  rec_clause <- if (is.finite(rec)) {
    sprintf("se recomienda N = %s%s", format(round(rec)), ci_txt)
  } else {
    "el N recomendado no se alcanza en la grilla (extender 'sample_sizes')"
  }

  message(sprintf(
    "Para %d items en %d comunidades (tamanos %s), datos %s, power_target = %.2f, %s.",
    s$n_items, s$n_communities,
    paste(s$community_sizes, collapse = "-"),
    set$data_type,
    x$interpolation$target,
    rec_clause
  ))
  invisible(rec)
}


#' Visualize all plots stored in a \code{sample_size_EGA_montecarlo} object
#'
#' Prints each plot in \code{x$plots} sequentially. Use this after calling
#' \code{sample_size_EGA_montecarlo(..., plots = TRUE)} to inspect the
#' simulation graphically without having to write each \code{print()}.
#'
#' @param x A \code{sample_size_EGA_montecarlo} object that was produced with
#'   \code{plots = TRUE}, or its \code{$plots} list.
#' @param which Optional character vector with the names of the plots to
#'   show (subset of \code{names(x$plots)}). Default \code{NULL} = all.
#' @return Invisibly returns the list of plots.
#' @export
#' @examples
#' res <- sample_size_EGA_montecarlo(
#'   community_sizes = c(4, 4), within_edge = 0.20,
#'   sample_sizes = c(100, 200, 400), n_rep = 10,
#'   data_type = "continuous", seed = 1, verbose = FALSE, plots = TRUE,
#'   control = list(boots = 100, corr = "pearson", algorithm = "walktrap")
#' )
#' show_plots(res, which = "power_curve_ci")
show_plots <- function(x, which = NULL) {
  plots <- if (inherits(x, "sample_size_EGA_montecarlo")) x$plots else x
  if (is.null(plots) || length(plots) == 0) {
    message("No plots stored. Re-run sample_size_EGA_montecarlo(..., plots = TRUE).")
    return(invisible(NULL))
  }
  keep <- if (is.null(which)) names(plots) else intersect(which, names(plots))
  for (nm in keep) {
    p <- plots[[nm]]
    if (is.null(p)) next
    message("Plot: ", nm)
    print(p)
  }
  invisible(plots)
}
