#' Plot a single network with R2 progress rings
#'
#' Creates a ggplot of a single network with nodes colored by domain,
#' R2 progress rings, gradient-colored edges, and optional abbreviated
#' labels with a legend mapping.
#'
#' @param mat Adjacency matrix.
#' @param r2 Numeric vector of R2 values per node.
#' @param labels Character vector of node labels (default: colnames of mat).
#' @param short_labels Optional character vector of abbreviations displayed inside nodes.
#' @param full_labels Optional character vector of full names for the caption legend.
#' @param groups_list Named list mapping domain names to node indices.
#' @param group_cols Named character vector of colors per domain.
#' @param node_radius Numeric radius of node circles.
#' @param node_label_size Text size for node labels (NULL = auto).
#' @param ring_offset Gap between node circle and R2 ring.
#' @param ring_stroke Line width of the R2 ring.
#' @param ring_progress_col Color of the R2 progress arc.
#' @param edge_min Minimum absolute edge weight to display.
#' @param lwd_range Numeric vector of length 2: min and max line widths.
#' @param alpha_range Numeric vector of length 2: min and max alpha for edges.
#' @param edge_color_pos Color for the strongest positive edge.
#' @param edge_color_neg Color for the strongest negative edge.
#' @param edge_curvature Curvature of edges (0 = straight).
#' @param layout Layout algorithm: "spring" (Fruchterman-Reingold), "circle", or a Nx2 matrix.
#' @param layout_repulsion Repulsion parameter for FR layout (higher = more spread).
#' @param show_edge_values Logical; show edge weight labels?
#' @param edge_label_digits Number of decimal digits for edge labels.
#' @param edge_label_size Text size for edge labels.
#' @param edge_label_offset Perpendicular offset for edge labels.
#' @param edge_label_bg Logical; use background box for edge labels?
#' @param edge_label_bg_fill Fill color of the edge label background.
#' @param edge_label_bg_alpha Alpha of the edge label background.
#' @param show_R2_text Logical; show R2 numeric text near nodes?
#' @param R2_text_size Text size for R2 labels (NULL = auto).
#' @param R2_text_color Color for R2 labels.
#' @param R2_digits Decimal digits for R2 labels.
#' @param R2_position Position of R2 text: "below", "above", "right", "left".
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Caption text (default explains R2 ring).
#'
#' @return A ggplot object.
#' @export
#' @importFrom dplyr %>% tibble filter mutate left_join rename select transmute bind_rows
#' @importFrom purrr imap_dfr
#' @importFrom ggplot2 ggplot aes geom_curve geom_segment scale_size geom_point scale_color_manual guides guide_legend scale_fill_manual geom_text geom_label coord_equal theme_void theme labs element_text annotation_custom
#' @importFrom ggforce geom_circle geom_arc
#' @importFrom qgraph qgraph
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' plot_net(
#'   mat = network$graph,
#'   r2  = R2_values,
#'   labels = colnames(network$graph),
#'   short_labels = c("AE","HZ","CT"),
#'   full_labels  = c("Agotamiento Emocional","Hartazgo","Contraste")
#' )
#' }
plot_net <- function(
    mat,
    r2,
    labels = colnames(mat),
    short_labels = NULL,
    full_labels  = NULL,
    groups_list = NULL,
    group_cols = NULL,
    palette = "Darjeeling1",
    node_radius = 0.20,
    node_label_size = NULL,
    ring_offset = 0.045,
    ring_stroke = 2.4,
    ring_progress_col = "black",
    edge_min = 0.00,
    lwd_range = c(0.4, 4.5),
    alpha_range = c(0.15, 1),
    edge_color_pos = NULL,
    edge_color_neg = NULL,
    edge_curvature = 0.2,
    layout = "spring",
    layout_repulsion = 1,
    show_edge_values   = TRUE,
    edge_label_digits  = 2,
    edge_label_size    = 3.2,
    edge_label_offset  = 0.05,
    edge_label_bg      = TRUE,
    edge_label_bg_fill = "white",
    edge_label_bg_alpha= 0.9,
    show_R2_text   = TRUE,
    R2_text_size   = NULL,
    R2_text_color  = "grey30",
    R2_digits      = 2,
    R2_position    = "below",
    title    = NULL,
    subtitle = NULL,
    caption  = "Anillo exterior: proporci\u00f3n de varianza explicada (R\u00b2)"
){

  if (is.null(labels)) labels <- colnames(mat)

  # --- Auto-generar colores de nodos y aristas desde la paleta ---------------
  needs_node_cols <- is.null(group_cols)
  needs_edge_cols <- is.null(edge_color_pos) || is.null(edge_color_neg)

  if (needs_node_cols || needs_edge_cols) {
    n_groups <- if (!is.null(groups_list)) length(groups_list) else 1
    n_total  <- n_groups + 2
    all_cols <- wesanderson::wes_palette(palette, n_total, type = "continuous")
    all_rgb  <- grDevices::col2rgb(all_cols)
    warmth   <- all_rgb["red", ] - all_rgb["blue", ]
    ord      <- order(warmth)  # indice de mas frio a mas calido

    if (needs_edge_cols) {
      if (is.null(edge_color_pos)) edge_color_pos <- all_cols[ord[1]]
      if (is.null(edge_color_neg)) edge_color_neg <- all_cols[ord[n_total]]
    }
    if (needs_node_cols && !is.null(groups_list)) {
      mid_idx    <- ord[2:(n_total - 1)]
      group_cols <- stats::setNames(all_cols[mid_idx], names(groups_list))
    } else if (needs_node_cols) {
      group_cols <- c(Nodos = "#95A5A6")
    }
  }

  # --- Abreviaturas + leyenda de nombres ------------------------------------
  display_labels <- if (!is.null(short_labels)) short_labels else labels
  if (!is.null(short_labels) && !is.null(full_labels)) {
    legend_items <- paste0(short_labels, " = ", full_labels)
    n_per_row <- 4
    chunks <- split(legend_items, ceiling(seq_along(legend_items) / n_per_row))
    legend_txt <- paste(
      vapply(chunks, function(ch) paste(ch, collapse = "  |  "), character(1)),
      collapse = "\n"
    )
    caption <- if (!is.null(caption)) paste0(caption, "\n", legend_txt) else legend_txt
  }

  # --- Tamano auto de etiquetas ----------------------------------------------
  if (is.null(node_label_size)) node_label_size <- node_radius * 20
  if (is.null(R2_text_size))    R2_text_size    <- node_radius * 12

  # --- Layout (Fruchterman-Reingold u otro) ----------------------------------
  if (is.matrix(layout)) {
    L <- layout
  } else if (layout == "spring") {
    L <- qgraph::qgraph(mat, layout = "spring", DoNotPlot = TRUE,
                         repulsion = layout_repulsion)$layout
  } else if (layout == "circle") {
    L <- qgraph::qgraph(mat, layout = "circle", DoNotPlot = TRUE)$layout
  } else {
    L <- qgraph::qgraph(mat, layout = layout, DoNotPlot = TRUE,
                         repulsion = layout_repulsion)$layout
  }

  clamp01 <- function(x) pmin(pmax(x, 0), 1)

  nodes <- dplyr::tibble(node = seq_along(labels),
                          label = display_labels,
                          x = L[,1], y = L[,2],
                          R2 = clamp01(r2))

  if (!is.null(groups_list)) {
    dom_df <- purrr::imap_dfr(groups_list, ~dplyr::tibble(node = .x, domain = .y))
    nodes <- nodes %>% dplyr::left_join(dom_df, by = "node")
  } else {
    nodes <- nodes %>% dplyr::mutate(domain = "Nodos")
    if (!"Nodos" %in% names(group_cols)) group_cols <- c(group_cols, Nodos="#95A5A6")
  }

  # --- Anillo R2 -------------------------------------------------------------
  ring_r <- node_radius + ring_offset
  start_angle <- -pi/2

  r2_off <- ring_r + 0.06
  r2_shift <- switch(R2_position,
    "below" = list(dx=0, dy=-r2_off, vj=1,   hj=0.5),
    "above" = list(dx=0, dy= r2_off, vj=0,   hj=0.5),
    "right" = list(dx= r2_off, dy=0, vj=0.5, hj=0),
    "left"  = list(dx=-r2_off, dy=0, vj=0.5, hj=1),
    list(dx=0, dy=-r2_off, vj=1, hj=0.5)
  )
  r2_labels <- nodes %>%
    dplyr::transmute(x = x + r2_shift$dx, y = y + r2_shift$dy,
                     label = paste0("R\u00b2=", format(round(R2, R2_digits),
                                                       nsmall = R2_digits)))

  # --- Aristas con clipping --------------------------------------------------
  nodes_xy <- nodes %>% dplyr::select(node, x, y)
  clip_r <- ring_r + 0.005

  idx <- which(upper.tri(mat), arr.ind = TRUE)
  edges <- dplyr::tibble(i = idx[,1], j = idx[,2], w = mat[idx]) %>%
    dplyr::filter(w != 0, abs(w) >= edge_min) %>%
    dplyr::mutate(abs_w = abs(w),
                  sign  = ifelse(w >= 0, "Pos", "Neg")) %>%
    dplyr::left_join(nodes_xy, by = c("i"="node")) %>% dplyr::rename(xi=x, yi=y) %>%
    dplyr::left_join(nodes_xy, by = c("j"="node")) %>% dplyr::rename(xj=x, yj=y)

  if (nrow(edges) > 0) {
    edges <- edges %>%
      dplyr::mutate(
        dx  = xj - xi, dy = yj - yi,
        len = sqrt(dx^2 + dy^2),
        ux  = ifelse(len > 0, dx/len, 0),
        uy  = ifelse(len > 0, dy/len, 0),
        x    = xi + ux * clip_r,
        y    = yi + uy * clip_r,
        xend = xj - ux * clip_r,
        yend = yj - uy * clip_r,
        tx = (xi + xj) / 2, ty = (yi + yj) / 2,
        nx = ifelse(len > 0, -dy/len, 0),
        ny = ifelse(len > 0,  dx/len, 0),
        s  = ifelse(sign == "Pos", 1, -1),
        lx = tx + s * edge_label_offset * nx,
        ly = ty + s * edge_label_offset * ny
      ) %>%
      dplyr::select(x, y, xend, yend, lx, ly, w, abs_w, sign)
  }

  maxW <- ifelse(nrow(edges) > 0, max(edges$abs_w), 1)

  # --- Color gradiente por intensidad ----------------------------------------
  if (nrow(edges) > 0) {
    lighten <- function(col, f = 0.85) {
      r <- col2rgb(col); grDevices::rgb(r[1]+(255-r[1])*f, r[2]+(255-r[2])*f, r[3]+(255-r[3])*f, max=255)
    }
    pos_ramp <- colorRamp(c(lighten(edge_color_pos), edge_color_pos))
    neg_ramp <- colorRamp(c(lighten(edge_color_neg), edge_color_neg))
    edges <- edges %>%
      dplyr::mutate(
        intensity  = abs_w / maxW,
        edge_col   = ifelse(sign == "Pos",
                            rgb(pos_ramp(intensity), maxColorValue = 255),
                            rgb(neg_ramp(intensity), maxColorValue = 255)),
        edge_alpha = alpha_range[1] + intensity * (alpha_range[2] - alpha_range[1]),
        lab = format(round(w, edge_label_digits), nsmall = edge_label_digits)
      )
  }

  # --- Datos de anillos ------------------------------------------------------
  ring_bg   <- nodes %>% dplyr::transmute(x0=x, y0=y, r=ring_r)
  ring_prog <- nodes %>% dplyr::transmute(x0=x, y0=y, r=ring_r,
                                          start=start_angle,
                                          end=start_angle + 2*pi*R2)

  pad <- ring_r + 0.12
  xr  <- range(nodes$x); yr <- range(nodes$y)

  # --- Construir ggplot ------------------------------------------------------
  p <- ggplot2::ggplot()

  if (nrow(edges) > 0) {
    if (edge_curvature != 0) {
      p <- p + ggplot2::geom_curve(
        data = edges,
        ggplot2::aes(x=x, y=y, xend=xend, yend=yend, size=abs_w),
        color = edges$edge_col, alpha = edges$edge_alpha,
        curvature = edge_curvature, lineend = "round", ncp = 15)
    } else {
      p <- p + ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(x=x, y=y, xend=xend, yend=yend, size=abs_w),
        color = edges$edge_col, alpha = edges$edge_alpha,
        lineend = "round")
    }
  }

  p <- p +
    ggplot2::scale_size(range=lwd_range, limits=c(0, maxW), guide="none") +
    ggplot2::geom_point(data = data.frame(x=Inf, y=Inf,
                                          tipo=c("Positiva","Negativa")),
                        ggplot2::aes(x=x, y=y, color=tipo), size=0, stroke=0) +
    ggplot2::scale_color_manual(values = c("Positiva" = edge_color_pos,
                                           "Negativa" = edge_color_neg),
                                name = "Arista") +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(size=3, linewidth=1.5, shape=15, alpha=1))) +
    ggforce::geom_circle(data=ring_bg,
                         ggplot2::aes(x0=x0, y0=y0, r=r),
                         colour="grey85", linewidth=ring_stroke) +
    ggforce::geom_arc(data=ring_prog,
                      ggplot2::aes(x0=x0, y0=y0, r=r, start=start, end=end),
                      colour=ring_progress_col, linewidth=ring_stroke,
                      lineend="round") +
    ggforce::geom_circle(data=nodes,
                         ggplot2::aes(x0=x, y0=y, r=node_radius, fill=domain),
                         colour="black", linewidth=0.7) +
    ggplot2::scale_fill_manual(values=group_cols, name="Dominios") +
    ggplot2::geom_text(data=nodes,
                       ggplot2::aes(x=x, y=y, label=label),
                       size=node_label_size, fontface="bold")

  if (show_R2_text) {
    p <- p + ggplot2::geom_text(data=r2_labels,
                                ggplot2::aes(x=x, y=y, label=label),
                                size=R2_text_size, color=R2_text_color,
                                fontface="italic",
                                vjust=r2_shift$vj, hjust=r2_shift$hj)
  }

  if (show_edge_values && nrow(edges) > 0) {
    if (edge_label_bg) {
      p <- p + ggplot2::geom_label(data=edges,
                                   ggplot2::aes(x=lx, y=ly, label=lab),
                                   color=edges$edge_col, fontface="bold",
                                   size=edge_label_size, label.size=0,
                                   fill=edge_label_bg_fill, alpha=edge_label_bg_alpha,
                                   label.padding=grid::unit(0.15, "lines"),
                                   label.r=grid::unit(0.12, "lines"),
                                   show.legend=FALSE)
    } else {
      p <- p + ggplot2::geom_text(data=edges,
                                  ggplot2::aes(x=lx, y=ly, label=lab),
                                  color=edges$edge_col, fontface="bold",
                                  size=edge_label_size, show.legend=FALSE)
    }
  }

  p <- p +
    ggplot2::coord_equal(xlim=c(xr[1]-pad, xr[2]+pad),
                         ylim=c(yr[1]-pad, yr[2]+pad), expand=FALSE) +
    ggplot2::theme_void(base_size=12) +
    ggplot2::theme(legend.position = "right")

  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + ggplot2::labs(title=title, subtitle=subtitle, caption=caption)
  }
  if (!is.null(title))
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(face="bold", size=14, hjust=0.5))
  if (!is.null(subtitle))
    p <- p + ggplot2::theme(plot.subtitle = ggplot2::element_text(size=11, hjust=0.5, color="grey40"))
  if (!is.null(caption))
    p <- p + ggplot2::theme(plot.caption = ggplot2::element_text(size=8, hjust=0.5,
                                                                  face="italic", color="grey50",
                                                                  lineheight=1.3))

  p
}
