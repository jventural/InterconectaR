#' Print plots without the warnings of their internal layers
#'
#' Several plotting functions of the package tag the returned object with the
#' class \code{silent_gg} or \code{silent_plot}. These print methods draw the
#' plot exactly as ggplot2 or patchwork would, but silence the warnings that
#' some layers emit while drawing (for example, rows removed outside a fixed
#' scale).
#'
#' @param x A plot returned by one of the plotting functions of the package.
#' @param ... Further arguments passed to the next print method.
#' @return Invisibly returns \code{x}.
#' @name silent_print
#' @keywords internal
NULL

#' @rdname silent_print
#' @export
print.silent_gg <- function(x, ...) {
  suppressWarnings(NextMethod())
  invisible(x)
}

#' @rdname silent_print
#' @export
print.silent_plot <- function(x, ...) {
  suppressWarnings(NextMethod())
  invisible(x)
}
