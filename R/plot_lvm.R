#' @title Plot Latent Variable Models
#' @description Creates visualizations for psychonetrics latent variable models.
#' @param x A psychonetrics model object.
#' @param plot Types of plots to create (network, loadings, residcors, residpcors, latents).
#' @param ask Logical, whether to ask before each plot.
#' @param rotation Rotation function (default promax).
#' @param vsize Node size vector (default c(5, 5)).
#' @param palette RColorBrewer palette name (default "Set1").
#' @param ... Additional arguments passed to qgraph.
#' @return Invisible list of qgraph objects.
#' @export
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom qgraph qgraph qgraph.loadings
#' @examples
#' \donttest{
#' if (requireNamespace("psychonetrics", quietly = TRUE)) {
#'   set.seed(123)
#'   n <- 300
#'   f1 <- rnorm(n)
#'   f2 <- 0.3 * f1 + rnorm(n)
#'   items <- data.frame(
#'     sapply(1:4, function(i) 0.7 * f1 + rnorm(n, 0, 0.7)),
#'     sapply(1:4, function(i) 0.7 * f2 + rnorm(n, 0, 0.7))
#'   )
#'   names(items) <- c(paste0("A", 1:4), paste0("B", 1:4))
#'
#'   lambda <- matrix(0, 8, 2)
#'   lambda[1:4, 1] <- 1
#'   lambda[5:8, 2] <- 1
#'   mod <- psychonetrics::lvm(items, lambda = lambda, verbose = FALSE)
#'   mod <- psychonetrics::runmodel(mod, verbose = FALSE)
#'   plot_lvm(mod, plot = c("loadings", "latents"))
#' }
#' }
plot_lvm <- function(
    x, # Objeto psychonetrics
    plot = c("network", "loadings", "residcors", "residpcors", "latents"),
    ask,
    rotation = promax,
    vsize = c(5, 5), # Tamano por defecto de los nodos, ahora como vector
    palette = "Set1", # Paleta de colores por defecto de Color Brewer
    ...
){

  if (missing(ask)) ask <- length(plot) > 1
  old_par <- par(ask = ask)
  on.exit(par(old_par), add = TRUE)

  # Verificar disponibilidad de la paleta y cargar colores
  available_palettes <- rownames(RColorBrewer::brewer.pal.info)
  if (!palette %in% available_palettes) {
    stop("Specified palette is not available in RColorBrewer.")
  }
  colors <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palette, "maxcolors"], palette)

  # Asignar nombres de filas a lambda si son NULL y coinciden las dimensiones
  if (is.null(rownames(x@modelmatrices$fullsample$lambda))) {
    if (nrow(x@modelmatrices$fullsample$lambda) == length(colnames(x@sample@covs$fullsample))) {
      rownames(x@modelmatrices$fullsample$lambda) <- colnames(x@sample@covs$fullsample)
    } else {
      stop("Dimensions of lambda and covs do not match.")
    }
  }

  # Correccion para acceder correctamente a las matrices de covarianzas
  obs <- which(rownames(x@modelmatrices$fullsample$lambda) %in% colnames(x@sample@covs$fullsample))

  # Verificar si obs es vacio
  if (length(obs) == 0) {
    stop("No matching observed variables found.")
  }

  # Correlaciones parciales residuales: omega_epsilon cuando psychonetrics la
  # calcula; si no, se derivan de la inversa de sigma_epsilon
  pcor <- x@modelmatrices$fullsample$omega_epsilon
  if (is.null(pcor)) {
    pcor <- qgraph::wi2net(solve(x@modelmatrices$fullsample$sigma_epsilon))
  }
  pcor <- as.matrix(pcor)

  # Definir 'shape' y asegurarse que 'vsize' es numerico
  shape <- rep("circle", ncol(pcor))
  shape[obs] <- "square"

  # Asegurarse de que vsize es un vector numerico del tamano adecuado
  if (length(vsize) == 1) {
    vsize <- rep(vsize, 2) # Asegura que vsize siempre tenga dos elementos
  }
  vsize <- as.numeric(vsize)

  Res <- list()

  if ("network" %in% plot){
    Res$network <- qgraph::qgraph(pcor, ..., title = "Estimated network", color = colors, shape = shape, layout = "spring", vsize = vsize[1])
  }

  if ("residpcors" %in% plot){
    Res$residpcors <- qgraph::qgraph(pcor[obs, obs], ..., title = "Estimated residual partial correlations", color = colors, shape = "square", layout = "spring", vsize = vsize[2])
  }

  if ("residcors" %in% plot){
    Res$residcors <- qgraph::qgraph(cov2cor(x@modelmatrices$fullsample$sigma_epsilon), ..., title = "Estimated residual correlations", color = colors, shape = "square", layout = "spring", vsize = vsize[2])
  }

  if ("loadings" %in% plot){
    load <- x@modelmatrices$fullsample$lambda
    # Rotate:
    rot <- rotation(load)
    if (is.matrix(rot)){
      load <- rot
      rotmat <- matrix(1, 1, 1)
    } else {
      load <- rot$loadings
      rotmat <- rot$rotmat
    }
    fCovs <- solve(rotmat) %*% x@modelmatrices$fullsample$sigma_zeta %*% t(solve(rotmat))
    rownames(load) <- rownames(x@modelmatrices$fullsample$lambda)[obs]
    Res$loadings <- qgraph::qgraph.loadings(load, factorCors = fCovs, ..., title = "Estimated net loadings", color = colors, labels = rownames(x@modelmatrices$fullsample$lambda)[obs], model = "reflective", vsize = vsize)
  }

  if ("latents" %in% plot){
    sigma_zeta <- x@modelmatrices[["fullsample"]][["sigma_zeta"]]
    latents_colors <- rep(colors, length.out = ncol(sigma_zeta))
    Res$latents <- qgraph::qgraph(sigma_zeta, ..., title = "Latent variables network", color = latents_colors, shape = "circle", layout = "spring", vsize = vsize[1])
  }

  invisible(Res)
}
