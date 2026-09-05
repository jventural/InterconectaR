# =====================================================================
#  NetPowerLab  ·  Tamaño de muestra para un modelo de red (GGM)
#  Envoltura interactiva de bootnet::netSimulator() y powerly::powerly()
#
#  Dr. José Ventura-León
#  R 4.4.1 · bootnet 1.8 · powerly 1.10.0 · qgraph 1.9.8 · shiny · bslib
# =====================================================================

library(shiny)
library(bslib)
library(ggplot2)

# bootnet, powerly y qgraph se usan SIEMPRE con :: y no se adjuntan:
# powerly exporta su propia validate() y enmascararía la de shiny.
for (p in c("bootnet", "powerly", "qgraph")) {
  if (!requireNamespace(p, quietly = TRUE))
    stop("Falta el paquete ", p, ". Instálalo con install.packages(\"", p, "\").")
}

# ---------------------------------------------------------------------
# PALETA  (la misma de SemPowerLab: azul marino academico y granate)
# ---------------------------------------------------------------------
COL <- list(
  ink    = "#14202E",
  navy   = "#1D3557",
  navy2  = "#2A4A73",
  slate  = "#55677D",
  line   = "#DCE3EB",
  paper  = "#F4F6F9",
  accent = "#9E2A2B",
  green  = "#2F6F4E",
  teal   = "#14746F",
  amber  = "#B45309",
  purple = "#6A4C93"
)

# colores de los instrumentos en el grafo
COL_INS <- c("#E8C07D", "#8FBC9B", "#E8A598", "#9BB8DA")

tema <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = COL$ink,
  primary = COL$navy, secondary = COL$slate,
  base_font = font_google("Source Sans 3", local = FALSE),
  heading_font = font_google("Source Serif 4", local = FALSE),
  "font-size-base" = "0.95rem"
)

CSS <- sprintf('
:root{ --ink:%s; --navy:%s; --navy2:%s; --slate:%s; --line:%s; --paper:%s; --accent:%s; }
body{ background:var(--paper); }

.app-head{
  background:linear-gradient(100deg, var(--ink) 0%%, var(--navy) 62%%, var(--navy2) 100%%);
  color:#fff; padding:22px 30px 20px 30px; border-bottom:4px solid var(--accent);
}
.app-head h1{ font-family:"Source Serif 4",Georgia,serif; font-weight:600;
  font-size:1.62rem; margin:0; letter-spacing:.2px; }
.app-head .sub{ font-size:.86rem; opacity:.82; margin-top:5px; }
.app-head .marca{ float:right; text-align:right; font-size:.74rem; opacity:.72;
  line-height:1.55; padding-top:5px; }

.bloque{ background:#fff; border:1px solid var(--line); border-radius:9px;
  padding:15px 16px 6px 16px; margin-bottom:15px; }
.bloque > .rot{ font-family:"Source Serif 4",Georgia,serif; font-weight:600;
  color:var(--navy); font-size:.95rem; margin:-2px 0 4px 0;
  border-bottom:1px solid var(--line); padding-bottom:7px; }
.bloque > .rot .n{ display:inline-block; width:20px; height:20px; line-height:20px;
  text-align:center; border-radius:50%%; background:var(--navy); color:#fff;
  font-size:.72rem; font-family:"Source Sans 3",sans-serif; margin-right:8px; vertical-align:1px; }
.ayuda{ font-size:.76rem; color:var(--slate); line-height:1.42; margin:-8px 0 12px 0; }
.form-label{ font-weight:600; font-size:.83rem; color:var(--ink); margin-bottom:3px; }
.form-control, .form-select{ font-size:.86rem; }

.cab-inst{ display:grid; grid-template-columns:1fr 62px; gap:7px; font-size:.68rem;
  text-transform:uppercase; letter-spacing:.5px; color:var(--slate); font-weight:700;
  margin:2px 0 5px 0; }
.cab-inst span:last-child{ text-align:center; }
.fila-inst{ display:grid; grid-template-columns:1fr 62px; gap:7px; align-items:start;
  margin-bottom:7px; }
.fila-inst .form-group{ margin-bottom:0; }
.fila-inst input{ font-size:.82rem; padding:5px 7px; }
.fila-inst input[type=number]{ text-align:center; }

.tarjetas{ display:grid; grid-template-columns:1.4fr 1fr 1fr 1fr; gap:14px; margin-bottom:16px; }
.tar{ background:#fff; border:1px solid var(--line); border-radius:9px;
  padding:14px 18px 13px 18px; border-top:4px solid var(--slate); }
.tar.clave{ border-top-color:var(--accent); }
.tar .rot{ font-size:.72rem; letter-spacing:.7px; text-transform:uppercase; color:var(--slate); }
.tar .val{ font-family:"Source Serif 4",Georgia,serif; font-weight:600; color:var(--ink);
  font-size:2.05rem; line-height:1.12; margin:4px 0 2px 0; }
.tar.clave .val{ color:var(--accent); font-size:2.5rem; }
.tar .pie{ font-size:.76rem; color:var(--slate); line-height:1.38; }

.nav-tabs .nav-link{ font-size:.87rem; font-weight:600; color:var(--slate); }
.nav-tabs .nav-link.active{ color:var(--navy); border-bottom:2px solid var(--accent); }
.card{ border:1px solid var(--line); border-radius:9px; }

.consola{ background:#16202B; color:#E6EDF3; border-radius:8px; padding:16px 18px;
  font-family:Consolas,"Courier New",monospace; font-size:.82rem; line-height:1.62;
  white-space:pre; overflow-x:auto; }
.consola .cmt{ color:#7FB08A; }

.tabla-s{ border-collapse:collapse; width:100%%; font-size:.87rem; }
.tabla-s th, .tabla-s td{ border:1px solid var(--line); padding:9px 10px; text-align:center; }
.tabla-s th{ background:var(--navy); color:#fff; font-weight:600; }
.tabla-s td.fila{ background:#F0F3F7; font-weight:600; }
.tabla-s tr.marca td{ background:#FBEDED; font-weight:700; }

.leyenda{ font-size:.79rem; color:var(--slate); line-height:1.5; margin-top:12px; }
.aviso{ background:#FBEDED; border-left:4px solid var(--accent); color:#7A2224;
  padding:12px 16px; border-radius:0 6px 6px 0; font-size:.87rem; }
.espera{ background:#F0F3F7; border-left:4px solid var(--navy); color:var(--slate);
  padding:12px 16px; border-radius:0 6px 6px 0; font-size:.87rem; }
.btn-calc{ background:var(--navy); border-color:var(--navy); font-weight:600; }
.btn-calc:hover{ background:var(--ink); border-color:var(--ink); }

.parrafo{ background:#fff; border-left:5px solid var(--navy); border-radius:0 8px 8px 0;
  padding:20px 24px; font-size:.95rem; line-height:1.86; text-align:justify; }
.tramo{ padding:1px 2px; border-radius:3px; font-weight:500;
  text-decoration:underline dotted currentColor; text-decoration-thickness:1.5px;
  text-underline-offset:4px; transition:opacity .15s ease, background-color .15s ease; }
.s1{ color:#2A6F97; } .s2{ color:#14746F; } .s3{ color:#B45309; }
.s4{ color:#6A4C93; } .s5{ color:#9E2A2B; } .s6{ color:#8A6D1D; }
.parrafo.enfoque .tramo{ opacity:.22; }
.parrafo.enfoque .tramo.on{ opacity:1; background:#F3F6FA; }
.chips-6{ display:flex; flex-wrap:wrap; gap:9px; margin-top:16px; }
.chip6{ flex:1 1 150px; background:#fff; border:1px solid var(--line);
  border-top:4px solid var(--slate); border-radius:7px; padding:9px 12px 8px 12px;
  font-size:.79rem; line-height:1.32; color:var(--slate); cursor:default;
  transition:transform .12s ease, box-shadow .12s ease; }
.chip6 b{ display:block; font-size:1.15rem; font-weight:700; margin-bottom:2px; }
.chip6:hover{ transform:translateY(-2px); box-shadow:0 4px 12px rgba(20,32,46,.12); }
.chip6.k1{ border-top-color:#2A6F97; } .chip6.k1 b{ color:#2A6F97; }
.chip6.k2{ border-top-color:#14746F; } .chip6.k2 b{ color:#14746F; }
.chip6.k3{ border-top-color:#B45309; } .chip6.k3 b{ color:#B45309; }
.chip6.k4{ border-top-color:#6A4C93; } .chip6.k4 b{ color:#6A4C93; }
.chip6.k5{ border-top-color:#9E2A2B; } .chip6.k5 b{ color:#9E2A2B; }
.chip6.k6{ border-top-color:#8A6D1D; } .chip6.k6 b{ color:#8A6D1D; }
', COL$ink, COL$navy, COL$navy2, COL$slate, COL$line, COL$paper, COL$accent)

# ---------------------------------------------------------------------
# LA RED DE REFERENCIA
#   Bloques de nodos (un bloque por instrumento). Dentro de un instrumento
#   las dimensiones se conectan mas y mas fuerte; entre instrumentos, menos:
#   esas son las aristas que responden la hipotesis de la tesis.
# ---------------------------------------------------------------------
red_bloques <- function(nodos, dens_intra = .40, dens_inter = .15,
                        peso_intra = .20, peso_inter = .13, semilla = 2026) {
  set.seed(semilla)
  k <- sum(nodos)
  bl <- rep(seq_along(nodos), nodos)
  W <- matrix(0, k, k)
  if (k >= 2) {
    for (i in 1:(k - 1)) for (j in (i + 1):k) {
      mismo <- bl[i] == bl[j]
      if (stats::runif(1) < if (mismo) dens_intra else dens_inter) {
        w <- if (mismo) peso_intra else peso_inter
        W[i, j] <- W[j, i] <- max(.05, stats::rnorm(1, w, w * .22))
      }
    }
  }
  # ningun instrumento puede quedar suelto: sin al menos un puente hacia otro
  # instrumento, ese bloque no aporta nada a la hipotesis del estudio
  if (length(nodos) > 1) {
    for (b in seq_along(nodos)) {
      dentro <- which(bl == b); fuera <- which(bl != b)
      if (all(W[dentro, fuera] == 0)) {
        i <- dentro[sample.int(length(dentro), 1)]
        j <- fuera[sample.int(length(fuera), 1)]
        W[i, j] <- W[j, i] <- max(.05, peso_inter)
      }
    }
  }

  # la matriz de precision implicada tiene que ser definida positiva
  n <- 0
  while (min(eigen(diag(k) - W, only.values = TRUE)$values) < 1e-4 && n < 80) {
    W <- W * .95; n <- n + 1
  }
  attr(W, "bloque") <- bl
  W
}

etiquetas_nodos <- function(nombres, nodos) {
  out <- character(0)
  for (i in seq_along(nombres)) {
    if (nodos[i] < 1) next
    base <- toupper(substr(gsub("[^A-Za-zÀ-ÿ0-9]", "", nombres[i]), 1, 4))
    out <- c(out, if (nodos[i] == 1) base else paste0(base, seq_len(nodos[i])))
  }
  out
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
fila_inst <- function(i, nombre, nodos) {
  div(class = "fila-inst",
      textInput(paste0("ins", i), NULL, nombre),
      numericInput(paste0("nod", i), NULL, nodos, min = 0, max = 15, step = 1))
}

ui <- page_fillable(
  theme = tema,
  tags$head(tags$style(HTML(CSS)), tags$title("NetPowerLab")),

  div(class = "app-head",
      div(class = "marca", HTML("Dr. José Ventura-León")),
      h1("NetPowerLab"),
      div(class = "sub",
          "Tamaño de muestra para un modelo de red de dimensiones  ·  ",
          tags$code("bootnet::netSimulator()", style = "color:#DCE3EB"), " y ",
          tags$code("powerly::powerly()", style = "color:#DCE3EB"))),

  layout_sidebar(
    sidebar = sidebar(
      width = 400, bg = COL$paper, padding = 14,

      div(class = "bloque",
          div(class = "rot", span(class = "n", "1"), "La red de referencia"),
          div(class = "cab-inst", span("Instrumento"), span("Nodos")),
          fila_inst(1, "PSQI", 7),
          fila_inst(2, "GAD-7", 1),
          fila_inst(3, "PHQ-9", 1),
          fila_inst(4, "", 0),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("dintra", "Densidad interna", .40, min = 0, max = 1, step = .05),
            numericInput("dinter", "Densidad entre", .15, min = 0, max = 1, step = .05)),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("pintra", "Peso interno", .20, min = .05, max = .6, step = .01),
            numericInput("pinter", "Peso entre", .13, min = .05, max = .6, step = .01)),
          numericInput("semilla", "Semilla", 2026, min = 1, max = 99999, step = 1),
          actionButton("gen", "Generar la red", class = "btn btn-outline-secondary btn-sm w-100"),
          div(class = "ayuda", style = "margin-top:9px",
              "Un bloque de nodos por instrumento. Dentro de un instrumento las dimensiones se
               conectan más y más fuerte; entre instrumentos, menos: esas son las aristas que
               responden la hipótesis. Esta red hace de tamaño del efecto, así que se declara
               a partir de la literatura previa.")
      ),

      div(class = "bloque",
          div(class = "rot", span(class = "n", "2"), "Simular con bootnet"),
          textInput("ncases", "Tamaños a evaluar", "100, 150, 200, 300, 500"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("nreps", "Réplicas", 100, min = 10, max = 500, step = 10),
            numericInput("nlevels", "Niveles Likert", 4, min = 2, max = 7, step = 1)),
          checkboxInput("ordinal", "Generar datos ordinales (escala Likert)", TRUE),
          layout_columns(
            col_widths = c(7, 5),
            selectInput("metodo", "Estimador",
                        c("EBICglasso" = "EBICglasso", "Correlación parcial" = "pcor",
                          "ggmModSelect" = "ggmModSelect"), selected = "EBICglasso"),
            numericInput("cores", "Núcleos",
                         max(1, min(8, parallel::detectCores() - 1)),
                         min = 1, max = max(1, parallel::detectCores()), step = 1)),
          actionButton("sim", "Simular", class = "btn btn-primary btn-calc w-100"),
          div(class = "ayuda", style = "margin-top:9px",
              "Los datos se generan ordinales, como los de una escala real: simularlos continuos
               da un resultado optimista que después no se cumple. Sin repartir en varios núcleos
               esto tarda minutos; con ellos, segundos.")
      ),

      div(class = "bloque",
          div(class = "rot", span(class = "n", "3"), "Recomendar N con powerly"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("rlow", "Rango desde", 50, min = 20, max = 5000, step = 10),
            numericInput("rup", "hasta", 2000, min = 50, max = 20000, step = 50)),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("samples", "Muestras", 25, min = 5, max = 60, step = 1),
            numericInput("reps", "Réplicas", 25, min = 5, max = 100, step = 1)),
          selectInput("measure", "Qué rendimiento se exige",
                      c("rho · parecido de los pesos" = "rho",
                        "sen · aristas recuperadas" = "sen",
                        "spe · ausencias respetadas" = "spe",
                        "mcc · acuerdo global" = "mcc"), selected = "rho"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("mvalue", "Al menos", .70, min = .1, max = .99, step = .05),
            numericInput("svalue", "En el % de muestras", .80, min = .5, max = .99, step = .05)),
          actionButton("pow", "Calcular N", class = "btn btn-primary btn-calc w-100"),
          div(class = "ayuda", style = "margin-top:9px",
              "Tres pasos: simula en varios tamaños, ajusta una curva monótona y la remuestrea
               para dar un intervalo. Con 25 × 25 tarda medio minuto.")
      )
    ),

    uiOutput("tarjetas"),

    navset_card_tab(
      id = "tabs",
      nav_panel("La red declarada", value = "red",
                plotOutput("grafo", height = "470px"),
                div(class = "leyenda", uiOutput("txt_red"))),

      nav_panel("bootnet: qué se recupera", value = "boot",
                uiOutput("salida_boot")),

      nav_panel("powerly: cuántos hacen falta", value = "pow",
                uiOutput("salida_pow")),

      nav_panel("Código R reproducible",
                uiOutput("codigo"),
                div(style = "margin-top:14px",
                    downloadButton("dl_codigo", "Descargar el .R",
                                   class = "btn btn-outline-secondary btn-sm")),
                div(class = "leyenda",
                    "La red se guarda con su semilla: ese bloque reproduce exactamente el
                     número que aparece en la tesis.")),

      nav_panel("Párrafo para la tesis",
                uiOutput("parrafo"),
                div(style = "margin-top:14px",
                    downloadButton("dl_texto", "Descargar el párrafo",
                                   class = "btn btn-outline-secondary btn-sm")))
    )
  )
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  d2 <- function(x) {
    s <- sprintf("%.2f", x)
    if (startsWith(s, "0.")) substring(s, 2) else s
  }

  instrumentos <- reactive({
    nom <- c(input$ins1, input$ins2, input$ins3, input$ins4)
    nod <- c(input$nod1, input$nod2, input$nod3, input$nod4)
    ok <- !is.na(nod) & nod >= 1 & nzchar(trimws(nom))
    list(nom = nom[ok], nod = as.integer(nod[ok]))
  })

  red <- eventReactive(input$gen, ignoreNULL = FALSE, {
    ins <- instrumentos()
    k <- if (length(ins$nod)) sum(ins$nod) else 0L
    if (length(ins$nod) < 2)
      validate("Declara al menos dos instrumentos con su nombre y sus nodos.")
    if (k < 4)
      validate("La red necesita al menos cuatro nodos en total.")
    if (k > 30)
      validate("Más de 30 nodos hace la simulación impracticable aquí.")
    W <- red_bloques(ins$nod, input$dintra, input$dinter,
                     input$pintra, input$pinter, input$semilla)
    dimnames(W) <- list(etiquetas_nodos(ins$nom, ins$nod), etiquetas_nodos(ins$nom, ins$nod))
    W
  })

  info_red <- reactive({
    W <- red(); k <- ncol(W)
    ar <- sum(W[upper.tri(W)] != 0)
    list(k = k, pares = choose(k, 2), aristas = ar,
         densidad = ar / choose(k, 2),
         pesos = if (ar > 0) range(W[W != 0]) else c(0, 0))
  })

  # ---- el grafo ----
  output$grafo <- renderPlot({
    W <- red(); ins <- instrumentos()
    grupos <- split(seq_len(ncol(W)), rep(ins$nom, ins$nod))
    grupos <- grupos[unique(rep(ins$nom, ins$nod))]
    op <- par(mar = c(4, 2, 3, 2)); on.exit(par(op))
    qgraph::qgraph(W, layout = "spring", groups = grupos,
                   color = COL_INS[seq_along(grupos)],
                   labels = colnames(W), label.cex = 1.05,
                   vsize = max(6, 13 - ncol(W) * .35),
                   edge.color = NULL, posCol = COL$navy2, negCol = COL$accent,
                   border.color = "#5A5A5A", border.width = 1.4,
                   legend = TRUE, legend.cex = .58, GLratio = 6.5,
                   title = sprintf("La red que se declara en model_matrix: %d nodos, %d pares posibles",
                                   ncol(W), choose(ncol(W), 2)),
                   title.cex = .9, mar = c(5, 5, 5, 5))
  }, res = 96)

  output$txt_red <- renderUI({
    i <- info_red()
    HTML(sprintf(
      "La red tiene <b>%d nodos</b> y <b>%d aristas verdaderas</b> de %d pares posibles
       (densidad %s); los pesos van de %s a %s. El grosor de cada línea es su peso: eso es
       lo que <i>rho</i> compara y lo que la muestra tiene que recuperar. Cambiar la semilla
       da otra red con la misma estructura, útil para ver cuánto depende el resultado de la
       red concreta que se declaró.",
      i$k, i$aristas, i$pares, d2(i$densidad), d2(i$pesos[1]), d2(i$pesos[2])))
  })

  # ---- bootnet ----
  ncases_v <- reactive({
    v <- suppressWarnings(as.numeric(strsplit(input$ncases, "[,;[:space:]]+")[[1]]))
    sort(unique(v[!is.na(v) & v >= 30]))
  })

  boot <- eventReactive(input$sim, {
    W <- red(); nc <- ncases_v()
    validate(need(length(nc) >= 2, "Escribe al menos dos tamaños de muestra."))
    gen <- if (isTRUE(input$ordinal))
      bootnet::ggmGenerator(ordinal = TRUE, nLevels = input$nlevels)
    else bootnet::ggmGenerator(ordinal = FALSE)
    nucleos <- max(1, min(input$cores, parallel::detectCores()))
    withProgress(message = "Simulando redes", value = 0, {
      # una llamada por tamaño: así la barra avanza de verdad
      partes <- lapply(seq_along(nc), function(i) {
        setProgress(value = (i - 1) / length(nc),
                    detail = sprintf("N = %s  (%d de %d)",
                                     format(nc[i], big.mark = " "), i, length(nc)))
        suppressWarnings(suppressMessages(as.data.frame(
          bootnet::netSimulator(input = W, dataGenerator = gen, nCases = nc[i],
                                nReps = input$nreps, default = input$metodo,
                                nCores = nucleos))))
      })
      setProgress(value = 1, detail = "resumiendo")
      d <- do.call(rbind, partes)
      ag <- aggregate(cbind(sensitivity, specificity, correlation) ~ nCases,
                      data = d, FUN = function(x) mean(x, na.rm = TRUE))
      list(crudo = d, agregado = ag)
    })
  })

  output$salida_boot <- renderUI({
    if (input$sim == 0)
      return(div(class = "espera",
                 "Pulsa «Simular» en el panel 2. La simulación toma la red declarada, genera
                  datos de cada tamaño, vuelve a estimar la red y compara lo estimado con lo
                  verdadero."))
    b <- boot(); ag <- b$agregado
    filas <- lapply(seq_len(nrow(ag)), function(i)
      tags$tr(tags$td(class = "fila", format(ag$nCases[i], big.mark = " ")),
              tags$td(d2(ag$sensitivity[i])), tags$td(d2(ag$specificity[i])),
              tags$td(d2(ag$correlation[i]))))
    tagList(
      layout_columns(
        col_widths = c(5, 7),
        div(tags$table(class = "tabla-s",
                       tags$tr(tags$th("N"), tags$th("Sensibilidad"),
                               tags$th("Especificidad"), tags$th("Correlación")),
                       filas),
            div(class = "leyenda",
                HTML("<b>Sensibilidad:</b> conexiones verdaderas recuperadas ·
                      <b>Especificidad:</b> ausencias respetadas ·
                      <b>Correlación:</b> parecido entre los pesos estimados y los verdaderos."))),
        plotOutput("curvas_boot", height = "330px")),
      div(class = "leyenda", uiOutput("txt_boot")))
  })

  output$curvas_boot <- renderPlot({
    b <- boot()
    d <- b$crudo[, c("nCases", "sensitivity", "specificity", "correlation")]
    largo <- do.call(rbind, lapply(c("sensitivity", "specificity", "correlation"), function(m)
      data.frame(nCases = d$nCases, medida = m, valor = d[[m]])))
    largo$medida <- factor(largo$medida,
                           levels = c("sensitivity", "specificity", "correlation"),
                           labels = c("Sensibilidad", "Especificidad", "Correlación"))
    ggplot(largo, aes(factor(nCases), valor)) +
      geom_boxplot(fill = "#EDF1F6", colour = COL$navy, outlier.size = .6,
                   linewidth = .45) +
      facet_wrap(~medida) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
      labs(x = "Número de casos", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = COL$line),
            strip.text = element_text(face = 2, colour = COL$navy),
            axis.title = element_text(colour = COL$slate, face = 2),
            axis.text = element_text(colour = COL$slate))
  }, res = 96)

  output$txt_boot <- renderUI({
    ag <- boot()$agregado
    i1 <- 1; i2 <- nrow(ag)
    HTML(sprintf(
      "Con %s participantes se recupera el %s de las conexiones verdaderas y la correlación con
       la red verdadera es de %s; con %s, esas cifras son %s y %s. La especificidad baja al
       crecer la muestra (%s → %s) porque el modelo empieza a incluir aristas pequeñas: no es un
       defecto, es el precio de ver más. Cada casilla es la distribución de %d réplicas, no un
       promedio: lo que importa es cuántas de ellas superan el umbral que vas a declarar.",
      format(ag$nCases[i1], big.mark = " "), d2(ag$sensitivity[i1]), d2(ag$correlation[i1]),
      format(ag$nCases[i2], big.mark = " "), d2(ag$sensitivity[i2]), d2(ag$correlation[i2]),
      d2(ag$specificity[i1]), d2(ag$specificity[i2]), input$nreps))
  })

  # ---- powerly ----
  pw <- eventReactive(input$pow, {
    W <- red()
    validate(need(input$rup > input$rlow + 50, "El rango superior tiene que ser mayor."))
    withProgress(message = "Buscando el tamaño de muestra", value = .1, {
      r <- tryCatch(
        suppressWarnings(suppressMessages(
          powerly::powerly(range_lower = input$rlow, range_upper = input$rup,
                           samples = input$samples, replications = input$reps,
                           measure = input$measure, measure_value = input$mvalue,
                           statistic = "power", statistic_value = input$svalue,
                           model = "ggm", model_matrix = W,
                           cores = max(1, min(input$cores, parallel::detectCores())),
                           verbose = FALSE))),
        error = function(e) list(error = conditionMessage(e)))
      incProgress(.85)
      r
    })
  })

  output$salida_pow <- renderUI({
    if (input$pow == 0)
      return(div(class = "espera",
                 "Pulsa «Calcular N» en el panel 3. powerly simula en varios tamaños dentro del
                  rango, ajusta una curva monótona al rendimiento y la remuestrea para dar la
                  recomendación con su intervalo."))
    r <- pw()
    if (!is.null(r$error)) return(div(class = "aviso", "powerly no pudo: ", r$error))
    rec <- r$recommendation
    tope <- rec[["50%"]] >= input$rup * .98
    tagList(
      layout_columns(
        col_widths = c(5, 7),
        div(
          div(class = "tar clave", style = "border-top-width:5px",
              div(class = "rot", "Recomendación"),
              div(class = "val", format(rec[["50%"]], big.mark = " ")),
              div(class = "pie", sprintf(
                "participantes, para %s ≥ %s en el %s %% de las muestras",
                input$measure, d2(input$mvalue), round(input$svalue * 100)))),
          div(class = "leyenda", style = "margin-top:14px",
              HTML(sprintf(
                "Intervalo del 95 %%: <b>%s a %s</b> participantes.<br>
                 Convergió: <b>%s</b> tras %d iteración(es), en %s segundos.<br>
                 Es una simulación estocástica: dos corridas con semillas distintas difieren
                 en algunas decenas de casos. Se reporta el valor obtenido y se redondea
                 hacia arriba.",
                format(rec[["2.5%"]], big.mark = " "), format(rec[["97.5%"]], big.mark = " "),
                if (isTRUE(r$converged)) "sí" else "no", r$iteration, round(r$duration, 1)))),
          if (tope) div(class = "aviso", style = "margin-top:12px",
                        "La recomendación sale pegada al extremo del rango: amplía
                         «hasta» y vuelve a calcular.")),
        plotOutput("curva_pow", height = "340px")),
      div(class = "leyenda",
          "Cada punto es la proporción de muestras simuladas de ese tamaño que alcanzan el
           rendimiento exigido. La línea horizontal es el criterio declarado y la banda vertical,
           el intervalo de la recomendación. El eje horizontal no cubre todo el rango que
           escribiste: muestra el tramo al que powerly fue acotando la búsqueda en sus
           iteraciones, que es donde está la respuesta."))
  })

  output$curva_pow <- renderPlot({
    r <- pw()
    validate(need(is.null(r$error), "Sin resultado."))
    d <- data.frame(N = r$range$partition, potencia = as.numeric(r$step_1$statistics))
    rec <- r$recommendation
    ggplot(d, aes(N, potencia)) +
      annotate("rect", xmin = rec[["2.5%"]], xmax = rec[["97.5%"]], ymin = -Inf, ymax = Inf,
               fill = COL$accent, alpha = .08) +
      geom_hline(yintercept = input$svalue, linetype = "22", colour = COL$accent,
                 linewidth = .7) +
      geom_vline(xintercept = rec[["50%"]], linetype = "22", colour = COL$accent,
                 linewidth = .7) +
      geom_point(size = 2.6, colour = COL$navy, alpha = .85) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                  colour = COL$navy, linewidth = .9) +
      scale_y_continuous("Muestras que alcanzan el criterio",
                         limits = c(0, 1.02), breaks = seq(0, 1, .2)) +
      scale_x_continuous("Participantes (N)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = COL$line),
            axis.title = element_text(colour = COL$slate, face = 2),
            axis.text = element_text(colour = COL$slate))
  }, res = 96)

  # al pulsar un boton se abre su pestaña: Shiny no calcula lo que no se ve
  observeEvent(input$sim, nav_select("tabs", "boot"), ignoreInit = TRUE)
  observeEvent(input$pow, nav_select("tabs", "pow"), ignoreInit = TRUE)

  # ---- tarjetas ----
  output$tarjetas <- renderUI({
    i <- info_red()
    rec <- if (input$pow > 0 && is.null(pw()$error)) pw()$recommendation[["50%"]] else NA
    div(class = "tarjetas",
        div(class = "tar clave", div(class = "rot", "N recomendado"),
            div(class = "val", if (is.na(rec)) "—" else format(rec, big.mark = " ")),
            div(class = "pie", if (is.na(rec))
              "pulsa «Calcular N» en el panel 3"
              else sprintf("%s ≥ %s en el %s %% de las muestras",
                           input$measure, d2(input$mvalue), round(input$svalue * 100)))),
        div(class = "tar", div(class = "rot", "Nodos"),
            div(class = "val", i$k),
            div(class = "pie", "una dimensión por nodo")),
        div(class = "tar", div(class = "rot", "Aristas verdaderas"),
            div(class = "val", i$aristas),
            div(class = "pie", sprintf("de %d pares posibles", i$pares))),
        div(class = "tar", div(class = "rot", "Densidad"),
            div(class = "val", d2(i$densidad)),
            div(class = "pie", "proporción de pares conectados")))
  })

  # ---- código reproducible ----
  codigo_txt <- reactive({
    ins <- instrumentos(); i <- info_red()
    nc <- paste(ncases_v(), collapse = ", ")
    paste0(
"library(bootnet)\nlibrary(powerly)\nlibrary(qgraph)\n\n",
"# ---- 1. la red de referencia -------------------------------------------\n",
"# Un bloque de nodos por instrumento: ", paste(sprintf("%s (%d)", ins$nom, ins$nod), collapse = ", "), "\n",
"red_bloques <- function(nodos, dens_intra, dens_inter, peso_intra, peso_inter, semilla) {\n",
"  set.seed(semilla)\n",
"  k <- sum(nodos); bl <- rep(seq_along(nodos), nodos); W <- matrix(0, k, k)\n",
"  for (i in 1:(k - 1)) for (j in (i + 1):k) {\n",
"    mismo <- bl[i] == bl[j]\n",
"    if (runif(1) < if (mismo) dens_intra else dens_inter) {\n",
"      w <- if (mismo) peso_intra else peso_inter\n",
"      W[i, j] <- W[j, i] <- max(.05, rnorm(1, w, w * .22))\n",
"    }\n  }\n",
"  # ningun instrumento puede quedar suelto: al menos un puente hacia otro\n",
"  if (length(nodos) > 1) for (b in seq_along(nodos)) {\n",
"    dentro <- which(bl == b); fuera <- which(bl != b)\n",
"    if (all(W[dentro, fuera] == 0)) {\n",
"      i <- dentro[sample.int(length(dentro), 1)]\n",
"      j <- fuera[sample.int(length(fuera), 1)]\n",
"      W[i, j] <- W[j, i] <- max(.05, peso_inter)\n",
"    }\n  }\n",
"  while (min(eigen(diag(k) - W, only.values = TRUE)$values) < 1e-4) W <- W * .95\n",
"  W\n}\n\n",
sprintf("red <- red_bloques(c(%s), dens_intra = %s, dens_inter = %s,\n                   peso_intra = %s, peso_inter = %s, semilla = %d)\n",
        paste(ins$nod, collapse = ", "), d2(input$dintra), d2(input$dinter),
        d2(input$pintra), d2(input$pinter), input$semilla),
sprintf("# %d nodos, %d aristas verdaderas de %d pares posibles\n\n", i$k, i$aristas, i$pares),
"# ---- 2. que se recupera en cada tamano (Epskamp et al., 2018) ----------\n",
"sim <- netSimulator(\n",
"  input         = red,\n",
sprintf("  dataGenerator = ggmGenerator(ordinal = %s%s),\n",
        if (isTRUE(input$ordinal)) "TRUE" else "FALSE",
        if (isTRUE(input$ordinal)) sprintf(", nLevels = %d", input$nlevels) else ""),
sprintf("  nCases        = c(%s),\n", nc),
sprintf("  nReps         = %d,\n", input$nreps),
sprintf("  default       = \"%s\",\n", input$metodo),
sprintf("  nCores        = %d\n)\n", max(1, min(input$cores, parallel::detectCores()))),
"aggregate(cbind(sensitivity, specificity, correlation) ~ nCases,\n",
"          data = as.data.frame(sim), FUN = mean)\n\n",
"# ---- 3. el N recomendado (Constantin et al., 2026) ---------------------\n",
"rec <- powerly(\n",
sprintf("  range_lower = %d, range_upper = %d,\n", input$rlow, input$rup),
sprintf("  samples = %d, replications = %d,\n", input$samples, input$reps),
sprintf("  measure         = \"%s\",\n", input$measure),
sprintf("  measure_value   = %s,\n", d2(input$mvalue)),
"  statistic       = \"power\",\n",
sprintf("  statistic_value = %s,\n", d2(input$svalue)),
"  model           = \"ggm\",\n",
"  model_matrix    = red,\n",
sprintf("  cores           = %d\n)\n", max(1, min(input$cores, parallel::detectCores()))),
"summary(rec)\n")
  })

  output$codigo <- renderUI({
    h <- htmltools::htmlEscape(codigo_txt())
    h <- gsub("(#[^\n]*)", '<span class="cmt">\\1</span>', h)
    div(class = "consola", HTML(h))
  })

  # ---- párrafo ----
  ROTULOS <- c("qué es un nodo", "parámetros del modelo", "método, paquete y red",
               "criterio declarado", "N recomendado", "plan del coeficiente CS")

  tramos <- reactive({
    ins <- instrumentos(); i <- info_red()
    rec <- if (input$pow > 0 && is.null(pw()$error)) pw()$recommendation[["50%"]] else NA
    multi <- ins$nom[ins$nod > 1]; uni <- ins$nom[ins$nod == 1]
    c(
      sprintf("La red se especificó a nivel de dimensiones y no de ítems: %s%s.",
              if (length(multi))
                sprintf("cada dimensión de %s constituyó un nodo",
                        paste(multi, collapse = " y ")) else "cada dimensión constituyó un nodo",
              if (length(uni))
                sprintf(" y las escalas unidimensionales (%s) se incorporaron mediante su puntaje total",
                        paste(uni, collapse = " y ")) else ""),
      sprintf("Con ello el modelo estima %d correlaciones parciales entre %d nodos.",
              i$pares, i$k),
      "El tamaño muestral se determinó mediante simulación, procedimiento recomendado para modelos de red porque el número de parámetros impide el análisis de potencia convencional; siguiendo a Constantin et al. (2026), se empleó el paquete powerly en R 4.4.1 sobre una red de referencia derivada de la literatura previa con los mismos instrumentos.",
      sprintf("Se especificó como requisito %s de al menos %s con la red verdadera en el %s %% de las muestras.",
              switch(input$measure,
                     rho = "una correlación", sen = "una sensibilidad",
                     spe = "una especificidad", mcc = "un coeficiente de correlación de Matthews"),
              d2(input$mvalue), round(input$svalue * 100)),
      if (is.na(rec))
        "El procedimiento recomendó [N] participantes."
      else sprintf("El procedimiento recomendó %d participantes (IC 95 %%: %d a %d), valor que se contrastó con el enfoque de Epskamp et al. (2018) mediante bootnet::netSimulator.",
                   rec, pw()$recommendation[["2.5%"]], pw()$recommendation[["97.5%"]]),
      "Una vez recogidos los datos, la estabilidad de los índices de centralidad se evaluará con el coeficiente CS por submuestreo de casos (1000 réplicas), adoptando CS ≥ .50 como criterio."
    )
  })

  parrafo_txt <- reactive(paste(tramos(), collapse = " "))

  output$parrafo <- renderUI({
    tr <- tramos()
    esc <- function(x) gsub("&", "&amp;", x, fixed = TRUE)
    cuerpo <- paste(sprintf('<span class="tramo s%d" data-k="%d">%s</span>',
                            seq_along(tr), seq_along(tr), esc(tr)), collapse = " ")
    div(
      div(class = "parrafo", id = "parrafoTesis", HTML(cuerpo)),
      div(class = "chips-6",
          lapply(seq_along(ROTULOS), function(i)
            div(class = paste0("chip6 k", i), `data-k` = i, tags$b(i), ROTULOS[i]))),
      tags$script(HTML("
        (function(){
          var p = document.getElementById('parrafoTesis');
          if (!p) return;
          function enfocar(k){
            p.classList.add('enfoque');
            p.querySelectorAll('.tramo').forEach(function(t){
              t.classList.toggle('on', t.getAttribute('data-k') === String(k));
            });
          }
          function soltar(){
            p.classList.remove('enfoque');
            p.querySelectorAll('.tramo.on').forEach(function(t){ t.classList.remove('on'); });
          }
          document.querySelectorAll('.chip6').forEach(function(c){
            var k = c.getAttribute('data-k');
            c.addEventListener('mouseenter', function(){ enfocar(k); });
            c.addEventListener('mouseleave', soltar);
          });
          p.querySelectorAll('.tramo').forEach(function(t){
            t.addEventListener('mouseenter', function(){ enfocar(t.getAttribute('data-k')); });
            t.addEventListener('mouseleave', soltar);
          });
        })();
      ")),
      div(class = "leyenda",
          HTML("Constantin, M. A., Schuurman, N. K., &amp; Vermunt, J. K. (2026). A general Monte
                Carlo method for sample size analysis in the context of network models.
                <i>Psychological Methods, 31</i>(3), 385–405. https://doi.org/10.1037/met0000555
                &nbsp;·&nbsp; Epskamp, S., Borsboom, D., &amp; Fried, E. I. (2018). Estimating
                psychological networks and their accuracy: A tutorial paper. <i>Behavior Research
                Methods, 50</i>(1), 195–212. https://doi.org/10.3758/s13428-017-0862-1")))
  })

  # ---- descargas ----
  output$dl_codigo <- downloadHandler(
    filename = function() sprintf("red_tamano_muestra_%s.R", format(Sys.Date(), "%Y%m%d")),
    content = function(file) writeLines(codigo_txt(), file, useBytes = TRUE)
  )
  output$dl_texto <- downloadHandler(
    filename = function() sprintf("parrafo_participantes_red_%s.txt", format(Sys.Date(), "%Y%m%d")),
    content = function(file) writeLines(parrafo_txt(), file, useBytes = TRUE)
  )
}

shinyApp(ui, server)
