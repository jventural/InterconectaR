#' Generate Likert-Style Threshold Patterns
#'
#' @description
#' Convenience helper to build per-item thresholds for the latent normal scale
#' used by \code{sample_size_EGA_montecarlo()} when \code{data_type =
#' "ordinal"}. Useful for simulating realistic Likert scenarios with floor or
#' ceiling effects rather than the unrealistic equiprobable default.
#'
#' Patterns are parameterized via a Beta distribution over the latent
#' cumulative scale: \code{shape1 = shape2 = 1} gives equiprobable categories,
#' \code{shape1 < shape2} concentrates mass on lower categories (floor),
#' \code{shape1 > shape2} concentrates mass on upper categories (ceiling).
#'
#' @param n_items Integer number of items.
#' @param categories Integer number of ordinal response categories (>= 2).
#' @param pattern One of \code{"equiprobable"}, \code{"floor"},
#'   \code{"ceiling"}, \code{"mixed"} (half of items floor, half ceiling,
#'   shuffled), or \code{"custom"}.
#' @param shape1 First Beta shape parameter, used when \code{pattern =
#'   "custom"}. Ignored otherwise.
#' @param shape2 Second Beta shape parameter, used when \code{pattern =
#'   "custom"}. Ignored otherwise.
#' @param seed Optional integer seed for the shuffling step in
#'   \code{pattern = "mixed"}.
#'
#' @return A list of length \code{n_items}, each element a numeric vector of
#'   \code{categories - 1} thresholds on the standard normal scale.
#' @export
#' @examples
#' # 18 items, 5 categories, all with floor effect
#' tau_floor <- make_likert_thresholds(18, 5, "floor")
#' tau_floor[[1]]
#'
#' # Half floor, half ceiling, randomly interleaved
#' tau_mixed <- make_likert_thresholds(18, 5, "mixed", seed = 1)
#'
#' # Custom skew via Beta(1.5, 4)
#' tau_custom <- make_likert_thresholds(18, 5, "custom",
#'                                       shape1 = 1.5, shape2 = 4)
make_likert_thresholds <- function(
    n_items,
    categories,
    pattern = c("equiprobable", "floor", "ceiling", "mixed", "custom"),
    shape1  = NULL,
    shape2  = NULL,
    seed    = NULL
) {
  pattern <- match.arg(pattern)
  if (!is.numeric(n_items)   || length(n_items)   != 1 || n_items   < 1)  stop("'n_items' must be a positive integer.")
  if (!is.numeric(categories) || length(categories) != 1 || categories < 2) stop("'categories' must be an integer >= 2.")

  k_minus_1 <- categories - 1L
  cum_break <- seq_len(k_minus_1) / categories

  shape_pair <- switch(
    pattern,
    equiprobable = c(1,   1),
    floor        = c(1,   3),
    ceiling      = c(3,   1),
    mixed        = NA,    # handled below
    custom       = {
      if (is.null(shape1) || is.null(shape2)) {
        stop("'shape1' and 'shape2' must be provided when pattern = 'custom'.")
      }
      c(shape1, shape2)
    }
  )

  threshold_vec_from_beta <- function(s1, s2) {
    stats::qnorm(stats::pbeta(cum_break, s1, s2))
  }

  if (pattern == "mixed") {
    tau_floor   <- threshold_vec_from_beta(1, 3)
    tau_ceiling <- threshold_vec_from_beta(3, 1)
    half        <- floor(n_items / 2)
    out <- vector("list", n_items)
    out[seq_len(half)]              <- replicate(half, tau_floor, simplify = FALSE)
    out[(half + 1L):n_items]        <- replicate(n_items - half, tau_ceiling, simplify = FALSE)
    if (!is.null(seed)) set.seed(seed)
    return(sample(out))
  }

  tau <- threshold_vec_from_beta(shape_pair[1], shape_pair[2])
  replicate(n_items, tau, simplify = FALSE)
}
