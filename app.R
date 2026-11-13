# =====================================================================
# Contrôle synthétique sur la SAE : appli Shiny (locale ou en ligne via Shinylive)
# Moteur : ta spécification (synth() de Synth, copié dans R/synth_vendored.R ;
# prédicteur = moyenne pré du résultat ; placebos et p-value comme
# SCtools::mspe.test).
# Carte : bibliothèque JavaScript Leaflet (www/leaflet, www/carte.js), sans le
# package R leaflet, pour alléger la version en ligne.
# Lancement : ouvrir ce fichier dans RStudio puis « Run App ».
# =====================================================================
library(shiny)
library(bslib)
source("R/synth_vendored.R", encoding = "UTF-8")
source("R/scm_core.R", encoding = "UTF-8")
source("R/data_prep.R", encoding = "UTF-8")



DEFAULT_DATA <- "data/sae.csv"
MSPE_LIMIT   <- 20     # exclusion des placebos extrêmes, comme mspe.test(mspe.limit = 20)
K_POOL_ETAB  <- 100    # au niveau établissement : 100 donneurs les plus proches
BLEU  <- "#2039b9"     # bleu EHESP (logo)
NOIR  <- "#1d1d1b"     # noir EHESP (logo)
GRIS  <- "#c9cdd2"
VERT  <- "#00aa3c"     # vert EHESP (en-têtes des encadrés)
TOUTES <- "(toutes)"
SYN    <- "Contrefactuel (contrôle synthétique)"

# Référentiel FINESS : raison sociale, catégorie, coordonnées (WGS84)
GEO <- utils::read.csv("data/finess_geo.csv", colClasses = c(FI = "character"), encoding = "UTF-8")

# ---- Graphiques en R de base (sans ggplot2, pour alléger la version en ligne) ----
# w = largeur affichée en pixels ; le texte et l'axe des x s'adaptent.
taille_txt <- function(w) if (w < 500) 0.9 else if (w < 800) 1 else 1.1
# Années affichées sur l'axe des x : toutes si la place le permet, sinon une sur 2, 3...
# (environ 60 pixels par étiquette)
annees_axe <- function(years, w) {
  pas <- max(1, ceiling(length(years) / max(2, floor(w / 60))))
  years[(years - min(years)) %% pas == 0]
}
# Cadre commun : fond, grille légère, axes, ligne pointillée avant la première année traitée
cadre_graph <- function(years, ylim, w, t0) {
  cx <- taille_txt(w)
  par(mar = c(2.6, 3.6, 1, 1), cex = cx, family = "sans", las = 1, mgp = c(3, 0.6, 0), tcl = 0)
  plot(NA, xlim = range(years), ylim = ylim, xlab = "", ylab = "", axes = FALSE)
  xb <- annees_axe(years, w); yb <- pretty(ylim)
  abline(h = yb, v = xb, col = "#ebebeb", lwd = 1)
  axis(1, at = xb, labels = xb, lwd = 0, cex.axis = 0.85, col.axis = "#4d4d4d")
  axis(2, at = yb, labels = format(yb, big.mark = " ", scientific = FALSE, trim = TRUE),
       lwd = 0, cex.axis = 0.85, col.axis = "#4d4d4d")
  abline(v = t0 - 1, lty = 2, col = "grey55")
}
hauteur_graph <- function(w) if (w < 500) 340 else 520

# Listes avec recherche (selectize, inclus dans shiny)
recherche <- function(...) modifyList(list(placeholder = "Rechercher", maxOptions = 5000), list(...))

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = BLEU, "border-radius" = "0.6rem"),
  tags$head(
    tags$link(rel = "stylesheet", href = "leaflet/leaflet.css"),
    tags$script(src = "leaflet/leaflet.js"),
    tags$style(HTML("
    .conteneur {max-width: 1100px; margin: 0 auto; padding: 24px 12px 48px;}
    .entete {display:flex; align-items:center; gap:24px; margin-bottom:20px;}
    .entete img {height: 115px;}
    .entete h1 {font-size: 1.7rem; margin: 0; color: #2039b9; font-weight: 600;}
    .entete p {margin: 4px 0 0; color: #555;}
    .card {margin-bottom: 20px; background: #f3f4f6; border: 1px solid #d5d8dd;}
    .card .card-header {color: #00aa3c !important; font-size: 1.35rem !important; font-weight: 600;
                  background: #f3f4f6; border-bottom: 2px solid #b9bec6;}
    .card > .nav-underline, .card-header-tabs {border-bottom: 2px solid #b9bec6;}
    .form-switch .form-check-label {white-space: nowrap;}
    .aide-carte {font-size: .9rem; color: #666; margin: 4px 0 8px;}
    #carte {height: 430px; border-radius: .4rem; z-index: 0;}
    .entete-res {background: #fff; border: 1px solid #d5d8dd; border-radius: .4rem;
                 padding: 10px 14px; margin: 12px 0 8px; font-size: 1.05rem;}
    .entete-res div {margin: 2px 0;}
    .entete-res .lib {color: #2039b9; font-weight: 600;}
    .entete-res .pval {margin-top: 6px; color: #444;}
    .tableau {overflow-x: auto;}
    .selectize-dropdown {z-index: 2000;}
    @media (max-width: 575px) {
      .entete img {height: 70px;} .entete h1 {font-size: 1.3rem;} .entete {gap: 14px;}
      #carte {height: 320px;}
      .entete-res {font-size: .95rem;}
    }
    .accordion {margin-bottom: 20px;}
    .accordion-item {background: #f3f4f6; border: 1px solid #d5d8dd;}
    .accordion-button, .accordion-button:not(.collapsed) {
      color: #00aa3c; font-size: 1.35rem; font-weight: 600;
      background: #f3f4f6; box-shadow: none; border-bottom: 2px solid #b9bec6;}
    .accordion-button:focus {box-shadow: none;}
                            "))),
  div(class = "conteneur",

      
      
      
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Titre
    div(class = "entete",
      tags$img(src = "logo_ehesp.svg", alt = "EHESP"),
      div(h1("Outil : Inférence Causale"),
          p("Évaluer l'effet d'une intervention"))),
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    accordion(
      open = FALSE,                      # fermé au démarrage ; TRUE pour l'ouvrir
      accordion_panel(
        "Etapes de l'évaluation",
        
        tags$ol(
          tags$li(style = "color:#2039b9", tags$span(style = "color:#212529", "Quelle question veut-on résoudre ?")),
          tags$li(style = "color:#2039b9", tags$span(style = "color:#212529", "Quelles données ?")),
          tags$li(style = "color:#2039b9", tags$span(style = "color:#212529", "Quelle stratégie empirique ?")),
          tags$li(style = "color:#2039b9", tags$span(style = "color:#212529", "Quels sont les résultats ?")),
          tags$li(style = "color:#2039b9", tags$span(style = "color:#212529", "Quelles recommandations ?"))
        ),
        p("Les étapes 1. et 2. sont déjà définies pour cette exercice."),
        p("Voir cours ", tags$b("Inférence causale"), " pour plus de détail.")
      )
    ),
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    card(
      card_header("Question de recherche"),
      card_body(
        p("Utilisez l'application ci-dessous pour évaluer l'impact d'un choc ", tags$i("X"),"sur une variable ", tags$i("Y"),
          ", en utilisant l'approche ", tags$i("synthetic control.")
          )
        
      )
    ),

    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    card(
      card_header("Données"),
      card_body(
        p(
          strong("Données utilisées : "),
          "Statistiques annuelles des établissements de santé (SAE)"
        ),
        p(
          strong("Granularité : "),
          "Année × établissement"
        ),
        p(
          strong("Période : "),
          "2013–2025"
        ),
        p(
          strong("Producteur : "),
          "Direction de la recherche, des études, de l'évaluation et des statistiques (DREES)"
        ),
        
        p(
          strong("Informations : "),
          "Enquête administrative exhaustive menée chaque année par la DREES auprès des établissements de santé publics et privés. Elle recueille des informations détaillées sur leur activité, leurs capacités d’accueil (lits et places), leurs équipements, leurs personnels et leur organisation."
        )
      )
    ),
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    card(
      card_header("Stratégie empirique"),
      card_body(
        p(
          strong("Stratégie empirique : "),
          "Synthetic Control (SCM)"
        ),
        p(
          strong("Approche : "),
          "Construit un contrefactuel en combinant plusieurs unités non traitées, pondérées de manière à reproduire au mieux la trajectoire de l'unité traitée avant le choc."
        ),
        p(
          strong("Estimation de l'effet : "),
          "L'effet du choc est mesuré par l'écart entre l'unité traitée et celle de son contrôle synthétique après le choc."
        ),
        p(
          strong("Référence : "),
          "Abadie, A., Diamond, A. & Hainmueller, J. (2010). ",
          tags$i("Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program."),
          " Journal of the American Statistical Association, 105(490), 493–505."
        )
      )
    ),

    # 2. Paramètres
    card(
      card_header("Paramètres"),
      card_body(
        layout_columns(col_widths = c(4),
          fileInput("file", "Données (CSV)", accept = ".csv",
                    buttonLabel = "Parcourir", placeholder = "SAE par défaut")),
        # Ordre : niveau, catégorie, raison sociale, unité traitée, année, variable.
        # Listes courtes en menu natif (selectize = FALSE) : pas de clavier sur téléphone.
        layout_columns(col_widths = c(4, 4, 4),
          radioButtons("level", "Niveau", c("Établissement" = "etab", "Département" = "dept"), inline = TRUE),
          conditionalPanel("input.level == 'etab'",
            selectInput("cat", "Catégorie", choices = TOUTES, selectize = FALSE)),
          conditionalPanel("input.level == 'etab'",
            selectizeInput("rs", "Raison sociale", choices = NULL, options = recherche()))
        ),
        layout_columns(col_widths = c(4, 4, 4),
          selectizeInput("treated", "Unité traitée", choices = NULL, options = recherche()),
          selectInput("t0", "Première année traitée", choices = NULL, selectize = FALSE),
          selectInput("outcome", "Variable de résultat", choices = NULL, selectize = FALSE)
        ),
        div(class = "aide-carte", "Cliquer sur la carte pour choisir l'unité traitée ; molette, pincement ou boutons +/− pour zoomer."),
        div(id = "carte"),
        br(),
        layout_columns(col_widths = c(4, 8),
          selectizeInput("excl", "Donneurs à exclure", choices = NULL, multiple = TRUE,
                         options = recherche(placeholder = "Aucun", plugins = list("remove_button"))),
          conditionalPanel("input.level == 'etab'",
            div(style = "padding-top: 30px;",
              checkboxInput("same_cat", "Comparer seulement à des établissements de la même catégorie (ex. un CHR avec d'autres CHR)", FALSE)))
        )
      )
    ),

    actionButton("run", "Lancer", class = "btn-primary btn-lg w-100 mb-3"),

    # 3. Résultats
    card(
      card_header("Résultats"),
      card_body(
        navset_underline(
          nav_panel("Résultat", div(
            uiOutput("entete_traj"),
            uiOutput("alerts"),
            input_switch("show_synth", "Afficher le contrefactuel", FALSE),
            input_switch("y0", "Axe des y à partir de 0", TRUE),
            plotOutput("p_traj", height = "auto"))),
          nav_panel("Placebos", div(
            uiOutput("entete_gap"),
            plotOutput("p_gap", height = "auto"))),
          nav_panel("Poids", div(class = "tableau", br(), tableOutput("t_w"))),
          nav_panel("Data", div(br(),
            downloadButton("dl", "Télécharger (CSV)", class = "btn-outline-primary mb-3"),
            div(class = "tableau", tableOutput("t_data")), uiOutput("notes")))
        )
      )
    )
  ),
  tags$script(src = "carte.js")
)

server <- function(input, output, session) {
  rv <- reactiveValues(df = load_sae(DEFAULT_DATA))
  observeEvent(input$file, {
    rv$df <- tryCatch(load_sae(input$file$datapath),
                      error = function(e) { showNotification(conditionMessage(e), type = "error"); rv$df })
  })

  # ---- Référentiel des établissements (SAE x FINESS) -------------------
  etabs <- reactive({
    df <- rv$df
    last <- df[order(df$AN), ]; last <- last[!duplicated(last$FI, fromLast = TRUE), ]
    e <- data.frame(FI = last$FI, dept = last$dept, last_an = last$AN,
                    size = if ("LIT_MCO" %in% names(last)) last$LIT_MCO else 0)
    e <- merge(e, GEO, by = "FI", all.x = TRUE)
    e$rs[is.na(e$rs) | e$rs == ""] <- e$FI[is.na(e$rs) | e$rs == ""]
    e$categ_niv3_lib[is.na(e$categ_niv3_lib)] <- "Non renseignée"
    e[order(e$last_an < max(df$AN), -e$size), ]      # du plus gros au plus petit, disparus en fin
  })

  # ---- Filtre catégorie : réduit raisons sociales, FI et carte ------------
  sel_or_all <- function(x) !is.null(x) && x != TOUTES
  filtered <- reactive({
    e <- etabs()
    if (sel_or_all(input$cat)) e <- e[e$categ_niv3_lib == input$cat, ]
    e
  })
  observe({
    cats <- c(TOUTES, sort(unique(etabs()$categ_niv3_lib))); cur <- isolate(input$cat)
    updateSelectInput(session, "cat", choices = cats, selected = if (!is.null(cur) && cur %in% cats) cur else TOUTES)
  })

  unit_choices <- reactive({
    if (input$level == "dept") sort(unique(rv$df$dept)) else filtered()$FI
  })
  # Libellés affichés dans les listes : « Manche (50) » pour un département
  avec_noms <- function(ch, level) if (level == "dept") setNames(ch, dept_label(ch)) else ch

  # Listes « Unité traitée » et « Raison sociale », mises à jour ensemble
  observe({
    ch <- unit_choices(); cur <- isolate(input$treated)
    sel <- if (!is.null(cur) && cur %in% ch) cur
           else if (input$level == "dept") (if ("50" %in% ch) "50" else ch[1])
           else { e <- filtered(); in50 <- e$FI[e$dept == "50"]; if (length(in50)) in50[1] else ch[1] }
    updateSelectizeInput(session, "treated", choices = avec_noms(ch, input$level), selected = sel)
    if (input$level == "etab") {
      e <- filtered()
      updateSelectizeInput(session, "rs", choices = sort(unique(e$rs)), selected = e$rs[match(sel, e$FI)])
    }
  })

  # Synchronisation dans les deux sens : FI -> raison sociale, raison sociale -> FI
  observeEvent(input$treated, {
    if (input$level != "etab") return()
    rs <- etabs()$rs[match(input$treated, etabs()$FI)]
    if (!is.na(rs) && !identical(rs, input$rs)) updateSelectizeInput(session, "rs", selected = rs)
  })
  observeEvent(input$rs, {
    if (input$level != "etab" || is.null(input$rs) || input$rs == "") return()
    e <- filtered()
    if (!identical(e$rs[match(input$treated, e$FI)], input$rs)) {
      fi <- e$FI[e$rs == input$rs][1]
      if (!is.na(fi)) updateSelectizeInput(session, "treated", selected = fi)
    }
  })
  observe({
    ch <- if (input$level == "dept") sort(unique(rv$df$dept)) else etabs()$FI
    updateSelectizeInput(session, "excl", choices = avec_noms(ch, input$level), selected = character(0))
  })
  observe({
    df <- rv$df; yrs <- sort(unique(df$AN)); nv <- numeric_vars(df)
    updateSelectInput(session, "t0", choices = yrs[-(1:2)], selected = yrs[ceiling(length(yrs) * 0.6)])
    updateSelectInput(session, "outcome", choices = nv, selected = nv[1])
  })

  # ---- Carte (www/carte.js) ----------------------------------------------
  # Points (établissements) ou contours (départements) selon le niveau et les filtres
  observe({
    if (input$level == "etab") {
      e <- filtered(); e <- e[!is.na(e$lat), ]
      cadre <- NULL                       # recadrage quand le filtre réduit la sélection
      if (nrow(e) > 0 && nrow(e) < nrow(etabs())) {
        em <- e[e$lat > 41 & e$lat < 52 & e$lon > -6 & e$lon < 10, ]
        if (nrow(em) == 0) em <- e
        cadre <- if (nrow(em) == 1) c(em$lat, em$lon)
                 else list(c(min(em$lat), min(em$lon)), c(max(em$lat), max(em$lon)))
      }
      session$sendCustomMessage("carte_etabs", list(
        id = I(e$FI), lat = I(e$lat), lon = I(e$lon),
        label = I(sprintf("%s · %s · %s", e$FI, e$rs, e$categ_niv3_lib)), cadre = cadre))
      sel_carte(isolate(input$treated), "etab", recentrer = FALSE)
    } else {
      session$sendCustomMessage("carte_depts", list(sel = isolate(input$treated)))
    }
  })

  # Mise en évidence de l'unité traitée
  sel_carte <- function(id, level, recentrer = TRUE) {
    if (is.null(id) || id == "") return()
    if (level == "dept") return(session$sendCustomMessage("carte_sel", list(type = "dept", code = id)))
    e <- etabs()[etabs()$FI == id, ]
    if (!nrow(e) || is.na(e$lat)) return(session$sendCustomMessage("carte_sel", list(type = "etab", lat = NULL)))
    session$sendCustomMessage("carte_sel", list(
      type = "etab", lat = e$lat, lon = e$lon,
      label = sprintf("%s · %s (unité traitée)", e$FI, e$rs),
      # recentrage seulement sans filtre actif (sinon le recadrage du filtre prime)
      recentrer = recentrer && !sel_or_all(isolate(input$cat))))
  }
  observeEvent(input$treated, sel_carte(input$treated, isolate(input$level)))

  # Clic sur la carte = choix de l'unité traitée
  observeEvent(input$carte_clic, {
    id <- input$carte_clic
    if (id %in% unit_choices()) updateSelectizeInput(session, "treated", selected = id)
  })

  # ---- Estimation ---------------------------------------------------------
  res <- eventReactive(input$run, {
    req(input$treated, input$t0, input$outcome)
    tryCatch({
      excl <- input$excl
      if (input$level == "etab" && isTRUE(input$same_cat)) {
        e <- etabs(); cat_t <- e$categ_niv3_lib[e$FI == input$treated]
        excl <- union(excl, e$FI[e$categ_niv3_lib != cat_t])
      }
      p <- prepare_panel(rv$df, input$level, input$outcome, character(0), input$treated,
                         as.integer(input$t0), scale = "level",
                         exclude_same_dept = TRUE, exclusions = excl,
                         k_pool = if (input$level == "etab") K_POOL_ETAB else Inf)
      if (input$level == "etab" && isTRUE(input$same_cat))
        p$notes <- c(p$notes, sprintf("Donneurs limités à la catégorie « %s ».", cat_t))
      fit <- scm_fit(p$Y, p$treated, p$donors, p$pre, NULL, lags = "mean", method = "synth")
      pl <- withProgress(message = "Calcul des placebos", value = 0,
        scm_placebos(p$Y, p$treated, p$donors, p$pre, NULL, lags = "mean", method = "synth",
                     progress = function(x) setProgress(x)))
      list(p = p, fit = fit, pl = pl, outcome = input$outcome, level = input$level,
           pv_all = scm_pvalue(fit, pl), pv_disc = scm_pvalue(fit, pl, MSPE_LIMIT),
           diag = scm_diagnostics(fit, p$Y, p$donors))
    }, error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
  })

  attente <- function() validate(need(input$run > 0, "Choisir les paramètres puis cliquer sur « Lancer »."))

  output$alerts <- renderUI({
    if (input$run == 0) return(NULL)
    r <- res(); if (is.null(r) || !length(r$diag)) return(NULL)
    lapply(r$diag, function(m) div(class = "alert alert-warning py-2 mb-2", m))
  })

  nom_unite <- function(r) {
    if (r$level == "dept") return(dept_label(r$fit$treated))   # ex. « Manche (50) »
    r$fit$treated
  }
  largeur <- function(id) { w <- session$clientData[[paste0("output_", id, "_width")]]
                            if (is.null(w) || w == 0) 800 else w }

  # Encadré au-dessus des graphiques : variable, raison sociale, unité traitée
  entete <- function(r, extra = NULL) {
    ligne <- function(lib, val) div(span(class = "lib", paste0(lib, " : ")), val)
    rs <- if (r$level == "etab") GEO$rs[match(r$fit$treated, GEO$FI)] else NA
    div(class = "entete-res",
        ligne("Outcome", r$outcome),
        if (!is.na(rs)) ligne("Raison sociale", rs),
        ligne("Unité traitée", nom_unite(r)),
        extra)
  }
  output$entete_traj <- renderUI({ req(input$run > 0); r <- req(res()); entete(r) })
  output$entete_gap <- renderUI({
    req(input$run > 0); r <- req(res())
    entete(r, div(class = "pval",
      sprintf("p-value = %.3f (rang %d sur %d, %d placebo(s) extrême(s) exclu(s)) ; sans exclusion : %.3f",
              r$pv_disc$p, r$pv_disc$rang, r$pv_disc$n, r$pv_disc$exclus, r$pv_all$p),
      br(), "Unité traitée en bleu, placebos en gris."))
  })

  output$p_traj <- renderPlot({
    attente(); r <- req(res()); f <- r$fit; w <- largeur("p_traj")
    synth <- isTRUE(input$show_synth)
    yr <- range(c(f$y, if (synth) f$synth), na.rm = TRUE)
    if (isTRUE(input$y0)) yr <- range(c(0, yr))
    lay <- if (synth) 2 else 1                     # légende : 1 ou 2 entrées
    etroit <- w < 600
    layout(matrix(1:2), heights = c(1, if (etroit) 0.11 * lay + 0.04 else 0.1))
    cadre_graph(f$years, yr, w, r$p$t0)
    lw <- if (w < 500) 2.2 else 3
    if (synth) lines(f$years, f$synth, col = NOIR, lwd = lw, lty = "42")
    lines(f$years, f$y, col = BLEU, lwd = lw)
    par(mar = c(0, 0, 0, 0)); plot.new()
    legend("center", legend = c("Unité traitée", if (synth) SYN), col = c(BLEU, if (synth) NOIR),
           lty = c("solid", if (synth) "42"), lwd = lw, bty = "n", seg.len = 2.5,
           horiz = !etroit, cex = (if (etroit) 1 else 0.9) * taille_txt(w), text.col = "#333333", x.intersp = 0.6, text.width = NA)
  }, height = function() hauteur_graph(largeur("p_traj")), res = 96)

  output$p_gap <- renderPlot({
    attente(); r <- req(res()); f <- r$fit; w <- largeur("p_gap")
    pl <- r$pl[vapply(r$pl, function(x) x$mspe_pre / f$mspe_pre < MSPE_LIMIT, logical(1))]
    gaps <- c(f$gap, unlist(lapply(pl, `[[`, "gap")))
    cadre_graph(f$years, range(c(0, gaps), na.rm = TRUE), w, r$p$t0)
    abline(h = 0, col = "grey55")
    for (x in pl) lines(f$years, x$gap, col = GRIS, lwd = 1)
    lines(f$years, f$gap, col = BLEU, lwd = if (w < 500) 2.4 else 3.2)
  }, height = function() hauteur_graph(largeur("p_gap")), res = 96)

  output$t_w <- renderTable({
    attente(); r <- req(res()); w <- r$fit$w; w <- sort(w[w > 0.001], decreasing = TRUE)
    d <- data.frame(Unité = names(w), Poids = round(w, 3))
    if (r$level == "etab") {
      e <- etabs()[match(d$Unité, etabs()$FI), ]
      d$`Raison sociale` <- e$rs; d$Catégorie <- e$categ_niv3_lib; d$Département <- e$dept
    } else d$Unité <- dept_label(d$Unité)
    d
  }, striped = TRUE, hover = TRUE, digits = 3)

  serie_data <- reactive({
    r <- req(res()); f <- r$fit
    data.frame(annee = f$years, periode = ifelse(f$years < r$p$t0, "avant", "après"),
               traite = round(f$y, 3), synthetique = round(f$synth, 3), ecart = round(f$gap, 3))
  })

  output$t_data <- renderTable({
    attente(); d <- serie_data()
    names(d) <- c("Année", "Période", "Traité", "Synthétique", "Écart")
    d
  }, striped = TRUE, hover = TRUE, digits = 3)

  output$dl <- downloadHandler(
    filename = function() { r <- res(); sprintf("scm_%s_%s_%d.csv", r$fit$treated, r$outcome, r$p$t0) },
    content = function(file) utils::write.csv(serie_data(), file, row.names = FALSE)
  )

  output$notes <- renderUI({
    req(input$run > 0); r <- req(res())
    tagList(h6("Traitement des données"), tags$ul(class = "text-muted", lapply(r$p$notes, tags$li)))
  })
}

shinyApp(ui, server)
