combine_groupBy <- function(
    red_group,
    plot_centralidad_group,
    bridge_plot_group = NULL,
    width_a = 8,
    width_bc = 4.5,
    show_plot = TRUE
) {
  # — 0) Instalar/cargar librerías necesarias --------------------------------
  required_packages <- c(
    "ggplot2",    # Para objetos ggplot
    "gridExtra",  # Para arrangeGrob
    "grid",       # Para grobs y viewports
    "cowplot",    # Para ggdraw y draw_label
    "ggplotify",  # Para convertir ggplot/grob a gtable
    "magick",     # Para image_graph e image_write (si red_group es magick-image)
    "png"         # Para readPNG
  )
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # — 1) Procesar 'red_group' según su tipo ----------------------------------
  # Si es una magick-image, leerla a rasterGrob; si es ggplot, convertir a grob;
  # si ya es grob/gtable, usarla directamente.
  if (inherits(red_group, "magick-image")) {
    temp_file <- tempfile(fileext = ".png")
    magick::image_write(red_group, path = temp_file)
    png_img   <- png::readPNG(temp_file)
    red_grob  <- grid::rasterGrob(png_img, interpolate = TRUE)
    unlink(temp_file)

  } else if (inherits(red_group, "ggplot")) {
    red_grob <- ggplotify::as.grob(red_group)

  } else if (
    inherits(red_group, "gtable") ||
    inherits(red_group, "grob") ||
    inherits(red_group, "zeroGrob")
  ) {
    red_grob <- red_group

  } else {
    stop("`red_group` debe ser un ggplot, un grob (grid) o una magick-image.")
  }

  # — 2) Crear etiquetas A, B y C --------------------------------------------
  label_a <- cowplot::ggdraw() +
    cowplot::draw_label("A", fontface = "bold", size = 14, x = 0.02, hjust = 0) +
    theme_void()
  label_b <- cowplot::ggdraw() +
    cowplot::draw_label("B", fontface = "bold", size = 14, x = 0.02, hjust = 0) +
    theme_void()
  label_c <- cowplot::ggdraw() +
    cowplot::draw_label("C", fontface = "bold", size = 14, x = 0.02, hjust = 0) +
    theme_void()

  # — 3) Preparar los subplots de centralidad (B) y bridge (C) ---------------
  b_plot <- cowplot::ggdraw(plot_centralidad_group) +
    theme(plot.margin = grid::unit(c(5, 5, 5, 5), "pt"))

  if (!is.null(bridge_plot_group)) {
    c_plot <- cowplot::ggdraw(bridge_plot_group) +
      theme(plot.margin = grid::unit(c(5, 5, 5, 5), "pt"))
  } else {
    c_plot <- cowplot::ggdraw() + theme_void()
  }

  # — 4) Construir la “columna derecha” (solo B, o B + C) ---------------------
  if (is.null(bridge_plot_group)) {
    right_col_grob <- gridExtra::arrangeGrob(
      gridExtra::arrangeGrob(
        label_b,
        b_plot,
        ncol    = 1,
        heights = c(0.1, 0.9)
      ),
      ncol = 1
    )
    rel_widths <- c(width_a, width_bc)

  } else {
    right_col_grob <- gridExtra::arrangeGrob(
      gridExtra::arrangeGrob(
        label_b,
        b_plot,
        ncol    = 1,
        heights = c(0.1, 0.9)
      ),
      gridExtra::arrangeGrob(
        label_c,
        c_plot,
        ncol    = 1,
        heights = c(0.1, 0.9)
      ),
      ncol    = 1,
      heights = c(0.5, 0.5)
    )
    rel_widths <- c(width_a, width_bc)
  }

  # — 5) Construir la “columna izquierda” (A + red_grob escalado) -------------
  # Para forzar que red_grob ocupe el 99% de la celda, lo envolvemos en un gTree
  # con un viewport que le da casi todo el espacio (0.99 "npc").
  raster_vp <- grid::gTree(
    children = grid::gList(red_grob),
    vp = grid::viewport(
      width  = grid::unit(0.99, "npc"),  # 99% del ancho de la celda
      height = grid::unit(0.99, "npc"),  # 99% de la altura de la celda
      just   = "centre"
    )
  )

  left_col_grob <- gridExtra::arrangeGrob(
    label_a,
    raster_vp,
    ncol    = 1,
    heights = c(0.05, 0.95)
  )

  # — 6) Unir izquierda y derecha en un solo gtable --------------------------
  combined_grob <- gridExtra::arrangeGrob(
    left_col_grob,
    right_col_grob,
    ncol   = 2,
    widths = rel_widths
  )

  # — 7) Convertir el gtable final a un objeto ggplot2 -----------------------
  combined_gg <- ggplotify::as.ggplot(combined_grob)

  # — 8) Mostrar en pantalla si show_plot = TRUE -----------------------------
  if (isTRUE(show_plot)) {
    print(combined_gg)
  }

  # — 9) Devolver el objeto ggplot2 listo para ggsave() ----------------------
  return(combined_gg)
}
