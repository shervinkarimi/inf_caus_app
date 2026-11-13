# =====================================================================
# Contrôle synthétique sur la SAE : appli Shiny locale
# Moteur : ta spécification (Synth::synth, prédicteur = moyenne pré du
# résultat, placebos et p-value comme SCtools::mspe.test).
# Lancement : ouvrir ce fichier dans RStudio puis « Run App ».
# =====================================================================
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(leaflet)
library(shinyWidgets)
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
DEPTS <- sf::st_read("data/departements.geojson", quiet = TRUE)   # contours des départements

theme_app <- theme_minimal(base_size = 18) +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        plot.subtitle = element_text(size = 17, hjust = 0.5, margin = margin(b = 12)),
        axis.text = element_text(size = 15), axis.title = element_text(size = 17))

picker_opts <- function(...) pickerOptions(liveSearch = TRUE, liveSearchPlaceholder = "Rechercher",
                                           noneResultsText = "Aucun résultat", size = 10, ...)

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = BLEU, "border-radius" = "0.6rem"),
  tags$head(tags$style(HTML("
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
    .bootstrap-select .dropdown-toggle {border: 1px solid #ced4da; background: #fff;}
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
          fileInput("file", "Données", accept = c(".csv", ".xlsx"),
                    buttonLabel = "Parcourir", placeholder = "SAE par défaut")),
        layout_columns(col_widths = c(4, 4, 4),
          radioButtons("level", "Niveau", c("Établissement" = "etab", "Département" = "dept"), inline = TRUE),
          selectInput("t0", "Première année traitée", choices = NULL),
          selectInput("outcome", "Variable de résultat", choices = NULL)
        ),
        layout_columns(col_widths = c(4, 4, 4),
          conditionalPanel("input.level == 'etab'",
            pickerInput("cat", "Catégorie", choices = TOUTES, options = picker_opts())),
          conditionalPanel("input.level == 'etab'",
            pickerInput("rs", "Raison sociale", choices = NULL, options = picker_opts())),
          pickerInput("treated", "Unité traitée", choices = NULL, options = picker_opts())
        ),
        div(class = "aide-carte", "Cliquer sur la carte pour choisir l'unité traitée ; molette ou boutons +/− pour zoomer."),
        leafletOutput("map", height = 430),
        br(),
        layout_columns(col_widths = c(4, 8),
          pickerInput("excl", "Donneurs à exclure", choices = NULL, multiple = TRUE,
                      options = picker_opts(actionsBox = TRUE, selectAllText = "Tout", deselectAllText = "Aucun",
                                            noneSelectedText = "Aucun", selectedTextFormat = "count > 3",
                                            countSelectedText = "{0} donneurs exclus")),
          conditionalPanel("input.level == 'etab'",
            div(style = "padding-top: 30px;",
              checkboxInput("same_cat", "Comparer seulement à des établissements de la même catégorie (ex. un CHR avec d'autres CHR)", FALSE)))
        )
      )
    ),

    actionButton("run", "Lancer", class = "btn-primary btn-lg w-100 mb-3"),

    # 3. Résultats
    navset_card_underline(
      nav_panel("Résultat", div(
        uiOutput("alerts"),
        input_switch("show_synth", "Afficher le contrefactuel", FALSE),
        input_switch("y0", "Axe des y à partir de 0", TRUE),
        plotOutput("p_traj", height = 520))),
      nav_panel("Placebos", plotOutput("p_gap", height = 520)),
      nav_panel("Poids", div(DTOutput("t_w"))),
      nav_panel("Data", div(
        downloadButton("dl", "Télécharger (CSV)", class = "btn-outline-primary mb-3"),
        DTOutput("t_data"), br(), uiOutput("notes")))
    )
  )
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
    updatePickerInput(session, "cat", choices = cats, selected = if (!is.null(cur) && cur %in% cats) cur else TOUTES)
  })

  unit_choices <- reactive({
    if (input$level == "dept") sort(unique(rv$df$dept)) else filtered()$FI
  })

  # Listes « Unité traitée » et « Raison sociale », mises à jour ensemble
  observe({
    ch <- unit_choices(); cur <- isolate(input$treated)
    sel <- if (!is.null(cur) && cur %in% ch) cur
           else if (input$level == "dept") (if ("50" %in% ch) "50" else ch[1])
           else { e <- filtered(); in50 <- e$FI[e$dept == "50"]; if (length(in50)) in50[1] else ch[1] }
    updatePickerInput(session, "treated", choices = ch, selected = sel)
    if (input$level == "etab") {
      e <- filtered()
      updatePickerInput(session, "rs", choices = sort(unique(e$rs)), selected = e$rs[match(sel, e$FI)])
    }
  })

  # Synchronisation dans les deux sens : FI -> raison sociale, raison sociale -> FI
  observeEvent(input$treated, {
    if (input$level != "etab") return()
    rs <- etabs()$rs[match(input$treated, etabs()$FI)]
    if (!is.na(rs) && !identical(rs, input$rs)) updatePickerInput(session, "rs", selected = rs)
  })
  observeEvent(input$rs, {
    if (input$level != "etab" || is.null(input$rs)) return()
    e <- filtered()
    if (!identical(e$rs[match(input$treated, e$FI)], input$rs)) {
      fi <- e$FI[e$rs == input$rs][1]
      if (!is.na(fi)) updatePickerInput(session, "treated", selected = fi)
    }
  })
  observe({
    ch <- if (input$level == "dept") sort(unique(rv$df$dept)) else etabs()$FI
    updatePickerInput(session, "excl", choices = ch, selected = character(0))
  })
  observe({
    df <- rv$df; yrs <- sort(unique(df$AN)); nv <- numeric_vars(df)
    updateSelectInput(session, "t0", choices = yrs[-(1:2)], selected = yrs[ceiling(length(yrs) * 0.6)])
    updateSelectInput(session, "outcome", choices = nv, selected = nv[1])
  })

  # ---- Carte ------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4, zoomSnap = 0.25, zoomDelta = 0.5)) |>
      addTiles() |>   # fond OpenStreetMap standard, sans clé d'accès
      addMapPane("selection", zIndex = 450) |>
      fitBounds(-5.2, 41.3, 9.6, 51.1)
  })

  add_depts <- function(m, selected) {
    s <- DEPTS$code == selected
    m |> clearGroup("dept") |>
      addPolygons(data = DEPTS, layerId = ~code, group = "dept",
                  color = ifelse(s, BLEU, "#7a7f87"), weight = ifelse(s, 2.5, 1),
                  fillColor = ifelse(s, BLEU, "#ffffff"), fillOpacity = ifelse(s, 0.55, 0.15),
                  label = ~sprintf("%s · %s", code, nom),
                  highlightOptions = highlightOptions(weight = 2.5, color = BLEU, bringToFront = TRUE))
  }

  # Points (établissements) ou contours (départements) selon le niveau et les filtres
  observe({
    m <- leafletProxy("map") |> clearGroup("etab") |> clearGroup("dept")
    if (input$level == "etab") {
      e <- filtered(); e <- e[!is.na(e$lat), ]
      m |> addCircleMarkers(data = e, lng = ~lon, lat = ~lat, layerId = ~FI, group = "etab",
                            radius = 5, stroke = TRUE, weight = 1, color = "#5b6068",
                            fillColor = "#9aa0a8", fillOpacity = 0.7,
                            label = ~sprintf("%s · %s · %s", FI, rs, categ_niv3_lib))
      # Recadrage quand le filtre réduit fortement la sélection
      if (nrow(e) > 0 && nrow(e) < nrow(etabs())) {
        em <- e[e$lat > 41 & e$lat < 52 & e$lon > -6 & e$lon < 10, ]
        if (nrow(em) == 0) em <- e
        if (nrow(em) == 1) m |> setView(em$lon, em$lat, zoom = 11)
        else m |> fitBounds(min(em$lon), min(em$lat), max(em$lon), max(em$lat))
      }
    } else {
      m |> clearGroup("sel") |> add_depts(isolate(input$treated)) |> fitBounds(-5.2, 41.3, 9.6, 51.1)
    }
  })

  # Mise en évidence de l'unité traitée
  observe({
    req(input$treated)
    m <- leafletProxy("map")
    if (input$level == "etab") {
      e <- etabs()[etabs()$FI == input$treated, ]
      m |> clearGroup("sel")
      if (nrow(e) && !is.na(e$lat)) {
        m |> addCircleMarkers(lng = e$lon, lat = e$lat, layerId = paste0("sel_", e$FI), group = "sel",
                              radius = 10, color = "#ffffff", weight = 2, fillColor = BLEU, fillOpacity = 1,
                              options = pathOptions(pane = "selection"),
                              label = sprintf("%s · %s (unité traitée)", e$FI, e$rs))
        # Recentrage seulement sans filtre actif (sinon le recadrage du filtre prime)
        b <- isolate(input$map_bounds); sans_filtre <- !sel_or_all(isolate(input$cat))
        if (sans_filtre && !is.null(b) && (e$lat > b$north || e$lat < b$south || e$lon > b$east || e$lon < b$west))
          m |> setView(e$lon, e$lat, zoom = 9)
      }
    } else {
      add_depts(m, input$treated)
    }
  })

  # Clic sur la carte = choix de l'unité traitée
  observeEvent(input$map_marker_click, {
    id <- sub("^sel_", "", input$map_marker_click$id)
    if (input$level == "etab" && id %in% unit_choices())
      updatePickerInput(session, "treated", selected = id)
  })
  observeEvent(input$map_shape_click, {
    code <- input$map_shape_click$id
    if (input$level == "dept" && !is.null(code) && code %in% unit_choices())
      updatePickerInput(session, "treated", selected = code)
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

  vline <- function(r) geom_vline(xintercept = r$p$t0 - 1, linetype = "dashed", colour = "grey55")
  nom_unite <- function(r) {
    if (r$level == "dept") return(paste("département", r$fit$treated))  # ex. « département 50 »
    rs <- GEO$rs[match(r$fit$treated, GEO$FI)]
    if (is.na(rs)) r$fit$treated else sprintf("%s (%s)", r$fit$treated, rs)
  }

  output$p_traj <- renderPlot({
    attente(); r <- req(res()); f <- r$fit
    d <- data.frame(year = f$years, val = f$y, serie = "Unité traitée")
    if (isTRUE(input$show_synth)) d <- rbind(d, data.frame(year = f$years, val = f$synth, serie = SYN))
    d$serie <- factor(d$serie, intersect(c("Unité traitée", SYN), d$serie))
    ggplot(d, aes(year, val, colour = serie, linetype = serie)) + vline(r) +
      geom_line(linewidth = 1.3) +
      scale_colour_manual(values = setNames(c(BLEU, NOIR), c("Unité traitée", SYN)), name = NULL) +
      scale_linetype_manual(values = setNames(c("solid", "42"), c("Unité traitée", SYN)), name = NULL) +
      (if (isTRUE(input$y0)) expand_limits(y = 0) else NULL) +
      scale_x_continuous(breaks = f$years) +
      labs(title = sprintf("Outcome : %s", r$outcome),
           subtitle = sprintf("Unité traitée : %s", nom_unite(r)),
           x = NULL, y = NULL) + theme_app
  })

  output$p_gap <- renderPlot({
    attente(); r <- req(res()); f <- r$fit
    pl <- r$pl[vapply(r$pl, function(x) x$mspe_pre / f$mspe_pre < MSPE_LIMIT, logical(1))]
    g <- ggplot() + vline(r) + geom_hline(yintercept = 0, colour = "grey55")
    if (length(pl)) {
      dp <- do.call(rbind, lapply(names(pl), function(j) data.frame(year = f$years, gap = pl[[j]]$gap, u = j)))
      g <- g + geom_line(data = dp, aes(year, gap, group = u), colour = GRIS, linewidth = 0.5)
    }
    g + geom_line(data = data.frame(year = f$years, gap = f$gap), aes(year, gap), colour = BLEU, linewidth = 1.4) +
      scale_x_continuous(breaks = f$years) +
      labs(title = sprintf("Écart traité moins synthétique : %s", r$outcome),
           subtitle = sprintf("Unité traitée en bleu, placebos en gris\np-value = %.3f (rang %d sur %d, %d placebo(s) extrême(s) exclu(s)) ; sans exclusion : %.3f",
                              r$pv_disc$p, r$pv_disc$rang, r$pv_disc$n, r$pv_disc$exclus, r$pv_all$p),
           x = NULL, y = NULL) + theme_app
  })

  output$t_w <- renderDT({
    attente(); r <- req(res()); w <- r$fit$w; w <- sort(w[w > 0.001], decreasing = TRUE)
    d <- data.frame(unite = names(w), poids = round(w, 3))
    if (r$level == "etab") {
      e <- etabs()[match(d$unite, etabs()$FI), ]
      d$raison_sociale <- e$rs; d$categorie <- e$categ_niv3_lib; d$departement <- e$dept
    }
    datatable(d, rownames = FALSE, options = list(pageLength = 15, dom = "tp"))
  })

  serie_data <- reactive({
    r <- req(res()); f <- r$fit
    data.frame(annee = f$years, periode = ifelse(f$years < r$p$t0, "avant", "après"),
               traite = round(f$y, 3), synthetique = round(f$synth, 3), ecart = round(f$gap, 3))
  })

  output$t_data <- renderDT({
    attente()
    datatable(serie_data(), rownames = FALSE, options = list(pageLength = 20, dom = "t"))
  })

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
