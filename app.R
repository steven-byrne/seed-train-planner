# Seed Train Planner
# An R Shiny app for planning mammalian cell culture seed trains
# (vial thaw -> shake flask expansion -> optional wave bioreactor -> production bioreactors).
# All cell lines, media and default values are generic examples.

library(shiny)
library(bslib)
library(DT)
library(plotly)

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

pal <- list(
  primary = "#0F766E",   # teal
  ink     = "#1E293B",
  muted   = "#64748B",
  line    = "#E2E8F0",
  surface = "#F8FAFC",
  stage   = c(Initiation = "#0EA5E9", Expansion = "#0F766E",
              Maintenance = "#F59E0B", Production = "#E11D48")
)

app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = pal$ink,
  primary = pal$primary,
  secondary = pal$muted,
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  "border-radius" = "0.6rem",
  "card-border-color" = pal$line
)

app_css <- "
  h1.bslib-page-title { font-size: 1.25rem; font-weight: 700; letter-spacing: -0.01em; margin: 0;
                        padding: .9rem 1.25rem; border-bottom: 1px solid #E2E8F0; background: #fff; }
  h1.bslib-page-title .fa { color: #0F766E; margin-right: .4rem; }
  .brand-sub { font-weight: 400; color: #64748B; font-size: 0.9rem; margin-left: .5rem; }
  .sidebar .accordion-button { font-weight: 600; font-size: 0.95rem; }
  .sidebar .form-label, .sidebar label { font-size: 0.85rem; color: #334155; }
  .sidebar .shiny-input-container { margin-bottom: .6rem; }
  .bslib-value-box .value-box-title { font-size: 0.8rem; text-transform: uppercase; letter-spacing: .04em; }
  .bslib-value-box .value-box-value { font-size: 1.6rem; font-weight: 700; }
  .bslib-value-box .value-box-title .fa, .bslib-value-box .value-box-title svg { color: #0F766E; margin-right: .35rem; }
  @media (max-width: 576px) { .brand-sub { display: none; } .bslib-value-box .value-box-value { font-size: 1.3rem; } }
  .strategy-card { border: 1px solid #E2E8F0; border-radius: .6rem; padding: 1rem 1.1rem;
                   cursor: pointer; transition: border-color .15s, box-shadow .15s; background: #fff; height: 100%; }
  .strategy-card:hover { border-color: #0F766E; box-shadow: 0 2px 10px rgba(15,118,110,.12); }
  .strategy-card.selected { border: 2px solid #0F766E; background: #F0FDFA; }
  .strategy-card h5 { font-size: 1rem; font-weight: 700; margin-bottom: .35rem; }
  .strategy-card p { color: #475569; font-size: .9rem; margin-bottom: .6rem; }
  .pill { display: inline-block; font-size: .75rem; font-weight: 600; padding: .15rem .55rem;
          border-radius: 999px; background: #E2E8F0; color: #334155; margin-right: .3rem; }
  .pill.on { background: #0F766E; color: #fff; }
  .empty-state { color: #64748B; text-align: center; padding: 2.5rem 1rem; }
  .empty-state .fa, .empty-state svg { font-size: 2rem; margin-bottom: .6rem; color: #94A3B8; }
  .equip-row { display: flex; justify-content: space-between; padding: .55rem 0; border-bottom: 1px solid #F1F5F9; }
  .equip-row:last-child { border-bottom: none; }
  .equip-row span:last-child { font-weight: 700; }
  table.dataTable thead th { font-size: .82rem; color: #475569; }
  table.dataTable tbody td { font-size: .88rem; }
"

# ---------------------------------------------------------------------------
# Calculation helpers (pure functions, no Shiny dependencies)
# ---------------------------------------------------------------------------

determine_vessel <- function(volume, use_wave, wave_max_volume) {
  if (volume <= 35) "125 mL flask"
  else if (volume <= 70) "250 mL flask"
  else if (volume <= 170) "500 mL flask"
  else if (volume <= 300) "1 L flask"
  else if (use_wave && volume <= wave_max_volume) "Wave bioreactor"
  else "5 L bioreactor"
}

calculate_strategies <- function(p) {
  bioreactor_volume_ml <- p$bioreactor_volume * 1000
  cells_per_bioreactor <- p$bioreactor_density * 1e6 * bioreactor_volume_ml
  total_cells_needed <- cells_per_bioreactor * p$bioreactor_count
  growth_72h <- 2^(72 / p$doubling_time)

  out <- list()
  bioreactors_per_wave <- 0

  # Wave bioreactor
  if (p$use_wave) {
    cells_from_wave <- p$wave_target_volume * p$passage_density * 1e6 * growth_72h
    bioreactors_per_wave <- floor(cells_from_wave / cells_per_bioreactor)
    if (bioreactors_per_wave >= 1) {
      waves <- ceiling(p$bioreactor_count / bioreactors_per_wave)
      out$wave <- list(
        name = "Wave bioreactor",
        description = sprintf("%d wave bioreactor(s) seed all %d production bioreactors.",
                              waves, p$bioreactor_count),
        waves_needed = waves, bioreactors_per_wave = bioreactors_per_wave)
    }
  }

  # Flasks only
  cells_from_flask <- p$flask_max_volume * p$passage_density * 1e6 * growth_72h
  flasks <- ceiling(total_cells_needed / cells_from_flask)
  if (flasks <= p$flask_max_count) {
    out$flasks <- list(
      name = "Flasks only",
      description = sprintf("%d x %g mL flask(s) seed the production bioreactors directly.",
                            flasks, p$flask_max_volume),
      flasks_needed = flasks)
  }

  # Flasks feeding a wave bioreactor
  if (p$use_wave && flasks > p$flask_max_count && bioreactors_per_wave >= 1) {
    waves <- ceiling(p$bioreactor_count / bioreactors_per_wave)
    cells_for_waves <- waves * p$wave_target_volume * p$passage_density * 1e6
    flasks_for_waves <- ceiling(cells_for_waves / cells_from_flask)
    if (flasks_for_waves <= p$flask_max_count) {
      out$combined <- list(
        name = "Flasks + wave bioreactor",
        description = sprintf("%d flask(s) seed %d wave bioreactor(s), which seed the production bioreactors.",
                              flasks_for_waves, waves),
        flasks_needed = flasks_for_waves, waves_needed = waves)
    }
  }

  # Multi-stage flasks
  if (flasks > p$flask_max_count && (!p$use_wave || length(out) == 0)) {
    stages <- 1
    n <- flasks
    while (n > p$flask_max_count && stages < 5) {
      stages <- stages + 1
      n <- ceiling(n / p$flask_max_count)
    }
    if (n <= p$flask_max_count) {
      out$multi_flask <- list(
        name = "Multi-stage flasks",
        description = sprintf("%d stages of flask cultures, ending with %d flasks.", stages, flasks),
        flasks_needed = flasks, flask_stages = stages)
    }
  }

  out
}

build_schedule <- function(p) {
  g24 <- 2^(24 / p$doubling_time)
  g72 <- 2^(72 / p$doubling_time)
  passage <- p$passage_density * 1e6
  threshold <- p$day1_dilution_threshold * 1e6
  thaw_density <- 10e6
  vessel <- function(v) determine_vessel(v, p$use_wave, p$wave_max_volume)

  thaw_date <- p$bioreactor_start - p$cell_age
  day1_date <- thaw_date + 1
  day4_date <- day1_date + 3
  day7_date <- day4_date + 3

  rows <- list(
    list("Thaw", "Initiation", thaw_date, "Thaw cell vial", thaw_density, thaw_density * g24,
         10, vessel(10), "Cell bank")
  )
  if (thaw_density >= threshold) {
    v <- min(35, thaw_density / passage)
    rows[[length(rows) + 1]] <- list("Day 1 dilution", "Expansion", day1_date, "Dilute culture",
                                     passage, passage * g72, v, vessel(v), "Thaw")
  }
  v4 <- if (thaw_density >= threshold) 100 else 50
  rows <- c(rows, list(
    list("Day 4 passage", "Expansion", day4_date, "Passage culture", passage, passage * g72,
         v4, vessel(v4), if (thaw_density >= threshold) "Day 1 dilution" else "Thaw"),
    list("Day 7 passage", "Expansion", day7_date, "Passage culture", passage, passage * g72,
         300, vessel(300), "Day 4 passage")
  ))
  if (isTRUE(p$use_backbone)) {
    rows[[length(rows) + 1]] <- list("Backbone culture", "Maintenance", day7_date + 3,
                                     "Maintain backbone culture", passage, passage * g72,
                                     300, "1 L flask", "Day 7 passage")
  }
  rows <- c(rows, list(
    list("Inoculum expansion", "Expansion", p$bioreactor_start - 6, "Expand inoculum",
         p$bioreactor_density * 1e6, p$bioreactor_density * 1e6 * g72, 300, "1 L flask",
         if (isTRUE(p$use_backbone)) "Backbone culture" else "Day 7 passage"),
    list("Bioreactor inoculation", "Production", p$bioreactor_start, "Inoculate production bioreactors",
         p$bioreactor_density * 1e6, NA, p$bioreactor_volume * 1000 * p$bioreactor_count,
         sprintf("%d x %g L bioreactor", p$bioreactor_count, p$bioreactor_volume), "Inoculum expansion")
  ))

  df <- do.call(rbind, lapply(rows, function(r) data.frame(
    Step = r[[1]], Stage = r[[2]], Date = r[[3]], Activity = r[[4]],
    Seed_density = r[[5]] / 1e6, Predicted_density = r[[6]] / 1e6,
    Volume_mL = round(r[[7]], 1), Vessel = r[[8]], Source = r[[9]],
    stringsAsFactors = FALSE)))
  df$Day <- as.integer(df$Date - p$bioreactor_start)
  df[order(df$Date), ]
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

num <- function(id, label, value, ...) numericInput(id, label, value, width = "100%", ...)

sidebar_inputs <- sidebar(
  width = 340,
  bg = pal$surface,
  accordion(
    id = "inputs", multiple = TRUE, open = c("Production run", "Growth and equipment"),
    accordion_panel(
      "Production run", icon = icon("flask"),
      layout_columns(col_widths = c(6, 6),
        num("experiment", "Experiment no.", 1001, min = 1, max = 9999, step = 1),
        dateInput("bioreactor_start", "Bioreactor start", value = Sys.Date() + 28, width = "100%")
      ),
      selectInput("cell_line", "Cell line",
                  c("CHO-K1 Clone A", "CHO-K1 Clone B", "CHO-S Clone C", "CHO-DG44 Clone D", "HEK293 Pool E"),
                  width = "100%"),
      selectInput("medium", "Medium",
                  c("Chemically Defined Medium A", "Chemically Defined Medium B", "Passage Medium", "Production Medium"),
                  width = "100%"),
      layout_columns(col_widths = c(6, 6),
        num("bioreactor_count", "Bioreactors", 4, min = 1, max = 24),
        num("bioreactor_volume", "Volume each (L)", 2, min = 0.25, max = 50, step = 0.25),
        num("bioreactor_density", HTML("Seed density (10<sup>6</sup>/mL)"), 0.5, min = 0.1, max = 2, step = 0.1),
        num("cell_age", "Cell age (days)", 21, min = 14, max = 60, step = 1)
      )
    ),
    accordion_panel(
      "Growth and equipment", icon = icon("chart-line"),
      num("doubling_time", "Doubling time (h)", 24, min = 10, max = 40),
      layout_columns(col_widths = c(6, 6),
        num("flask_max_volume", "Flask volume (mL)", 250, min = 50, max = 1000),
        num("flask_max_count", "Max flasks", 6, min = 1, max = 20)
      ),
      input_switch("use_wave", "Use a wave bioreactor", value = TRUE),
      conditionalPanel(
        "input.use_wave",
        layout_columns(col_widths = c(6, 6),
          num("wave_max_volume", "Wave max (mL)", 5000, min = 1000, max = 25000, step = 500),
          num("wave_target_volume", "Wave target (mL)", 4000, min = 1000, max = 25000, step = 500)
        )
      )
    ),
    accordion_panel(
      "Passage settings", icon = icon("vials"),
      layout_columns(col_widths = c(6, 6),
        num("vial_cells", HTML("Vial density (10<sup>6</sup>/mL)"), 10, step = 0.5),
        num("vial_volume", "Vial volume (mL)", 1, step = 0.1),
        num("thaw_density", HTML("Thaw density (10<sup>6</sup>/mL)"), 0.4, step = 0.05),
        num("thaw_volume", "Thaw volume (mL)", 30, step = 1),
        num("day1_dilution_threshold", HTML("Day 1 threshold (10<sup>6</sup>/mL)"), 0.8, step = 0.05),
        num("passage_density", HTML("Passage density (10<sup>6</sup>/mL)"), 0.3, step = 0.05)
      ),
      num("viability", "Expected viability (%)", 95, min = 50, max = 100),
      input_switch("use_backbone", "Keep a backbone culture", value = TRUE)
    )
  ),
  actionButton("calculate", "Calculate strategies", icon = icon("play"),
               class = "btn-primary btn-lg w-100 mt-2")
)

ui <- page_sidebar(
  title = tags$h1(class = "bslib-page-title", icon("seedling"), "Seed Train Planner",
                  tags$span(class = "brand-sub", "mammalian cell culture")),
  theme = app_theme,
  fillable = FALSE,
  sidebar = sidebar_inputs,
  tags$head(
    tags$title("Seed Train Planner"),
    tags$meta(name = "description", content = "Interactive R Shiny app for planning mammalian cell culture seed trains."),
    tags$meta(property = "og:title", content = "Seed Train Planner"),
    tags$meta(property = "og:description", content = "Plan a cell culture seed train backwards from the bioreactor start date: strategies, timeline and passage schedule."),
    tags$style(HTML(app_css))
  ),

  layout_column_wrap(
    width = "200px", fill = FALSE,
    value_box(tags$span(icon("dna"), "Cells required"), uiOutput("vb_cells"),
              theme = value_box_theme(bg = "#F0FDFA", fg = pal$ink)),
    value_box(tags$span(icon("droplet"), "Production volume"), textOutput("vb_volume"),
              theme = value_box_theme(bg = "#F0F9FF", fg = pal$ink)),
    value_box(tags$span(icon("snowflake"), "Thaw date"), textOutput("vb_thaw"),
              theme = value_box_theme(bg = "#FFFBEB", fg = pal$ink)),
    value_box(tags$span(icon("calendar-check"), "Bioreactor start"), textOutput("vb_start"),
              theme = value_box_theme(bg = "#FFF1F2", fg = pal$ink))
  ),

  card(
    card_header("Expansion strategies",
                tooltip(icon("circle-info"), "Routes that can deliver the required cells within your equipment limits. Click one to build its schedule.")),
    uiOutput("strategy_cards")
  ),

  navset_card_underline(
    id = "results",
    nav_panel("Timeline", icon = icon("timeline"),
              uiOutput("timeline_ui")),
    nav_panel("Schedule", icon = icon("table"),
              uiOutput("schedule_ui")),
    nav_panel("Equipment", icon = icon("boxes-stacked"),
              uiOutput("equipment_ui")),
    nav_spacer(),
    nav_item(actionLink("help", "How it works", icon = icon("circle-question")))
  ),

  tags$footer(class = "text-muted small mt-3 mb-2",
              "Independent personal project. Generic example values only. ",
              tags$a("Source on GitHub", href = "https://github.com/steven-byrne/seed-train-planner", target = "_blank"))
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  params <- reactive({
    keys <- c("bioreactor_count", "bioreactor_volume", "bioreactor_density", "cell_age",
              "doubling_time", "use_wave", "wave_max_volume", "wave_target_volume",
              "flask_max_volume", "flask_max_count", "passage_density",
              "day1_dilution_threshold", "use_backbone", "bioreactor_start")
    setNames(lapply(keys, function(k) input[[k]]), keys)
  })

  strategies <- reactiveVal(NULL)
  selected <- reactiveVal(NULL)

  # Live summary tiles
  output$vb_cells <- renderUI({
    p <- params(); req(p$bioreactor_count, p$bioreactor_volume, p$bioreactor_density)
    HTML(sprintf("%.1f &times; 10<sup>9</sup>", p$bioreactor_count * p$bioreactor_volume * p$bioreactor_density))
  })
  output$vb_volume <- renderText({
    p <- params(); req(p$bioreactor_count, p$bioreactor_volume)
    sprintf("%g L", p$bioreactor_count * p$bioreactor_volume)
  })
  output$vb_thaw <- renderText({
    p <- params(); req(p$bioreactor_start, p$cell_age)
    format(p$bioreactor_start - p$cell_age, "%a %d %b")
  })
  output$vb_start <- renderText({
    req(input$bioreactor_start); format(input$bioreactor_start, "%a %d %b")
  })

  observeEvent(input$calculate, {
    s <- calculate_strategies(params())
    strategies(s)
    if (length(s)) {
      selected(names(s)[1])
      showNotification(sprintf("%d strategy option(s) found", length(s)), type = "message", duration = 3)
    } else {
      selected(NULL)
      showNotification("No feasible strategy. Try more flasks or enable the wave bioreactor.",
                       type = "warning", duration = 6)
    }
  })

  observeEvent(input$pick_strategy, selected(input$pick_strategy))

  # Recalculate when inputs change after the first calculation
  observeEvent(params(), {
    if (!is.null(strategies())) {
      s <- calculate_strategies(params())
      strategies(s)
      if (!isTRUE(selected() %in% names(s))) selected(if (length(s)) names(s)[1] else NULL)
    }
  }, ignoreInit = TRUE)

  schedule <- reactive({
    req(selected())
    build_schedule(params())
  })

  empty_state <- function(icon_name, msg) div(class = "empty-state", icon(icon_name), p(msg))

  output$strategy_cards <- renderUI({
    s <- strategies()
    if (is.null(s)) return(empty_state("play", "Set your run in the sidebar, then click Calculate strategies."))
    if (!length(s)) return(empty_state("triangle-exclamation", "No feasible strategy with the current equipment limits."))
    cards <- lapply(names(s), function(id) {
      st <- s[[id]]
      pills <- tagList(
        if (!is.null(st$flasks_needed)) span(class = "pill", sprintf("%d flask(s)", st$flasks_needed)),
        if (!is.null(st$waves_needed)) span(class = "pill", sprintf("%d wave", st$waves_needed)),
        if (!is.null(st$flask_stages)) span(class = "pill", sprintf("%d stages", st$flask_stages))
      )
      div(class = paste("strategy-card", if (identical(selected(), id)) "selected"),
          onclick = sprintf("Shiny.setInputValue('pick_strategy', '%s', {priority: 'event'})", id),
          h5(st$name), p(st$description), pills,
          if (identical(selected(), id)) span(class = "pill on", icon("check"), "Selected"))
    })
    do.call(layout_column_wrap, c(list(width = "240px", fill = FALSE), cards))
  })

  output$timeline_ui <- renderUI({
    if (is.null(selected())) return(empty_state("timeline", "Choose a strategy to see its timeline."))
    plotlyOutput("timeline", height = "320px")
  })

  output$timeline <- renderPlotly({
    df <- schedule()
    stages <- c("Initiation", "Expansion", "Maintenance", "Production")
    df$Stage <- factor(df$Stage, levels = rev(stages))
    hover <- sprintf("<b>%s</b><br>%s<br>Day %d<br>%s mL, %s",
                     df$Step, format(df$Date, "%a %d %b %Y"), df$Day,
                     format(df$Volume_mL, big.mark = ","), df$Vessel)
    plot_ly(df, x = ~Date, y = ~Stage, type = "scatter", mode = "lines",
            line = list(color = "#CBD5E1", width = 2), hoverinfo = "skip", showlegend = FALSE) %>%
      add_trace(type = "scatter", mode = "markers+text",
                marker = list(size = 14, color = unname(pal$stage[as.character(df$Stage)]),
                              line = list(color = "#fff", width = 2)),
                text = ~Step, textposition = "top center", textfont = list(size = 11, color = pal$muted),
                hovertext = hover, hoverinfo = "text", showlegend = FALSE, inherit = FALSE,
                x = ~Date, y = ~Stage) %>%
      layout(xaxis = list(title = "", gridcolor = "#F1F5F9", tickformat = "%d %b",
                          range = c(min(df$Date) - 2, max(df$Date) + 3)),
             yaxis = list(title = "", gridcolor = "#F1F5F9",
                          categoryorder = "array", categoryarray = rev(stages)),
             margin = list(l = 10, r = 10, t = 30, b = 30),
             font = list(family = "Inter, sans-serif", color = pal$ink),
             plot_bgcolor = "#fff", paper_bgcolor = "#fff") %>%
      config(displayModeBar = FALSE)
  })

  output$schedule_ui <- renderUI({
    if (is.null(selected())) return(empty_state("table", "Choose a strategy to see its passage schedule."))
    tagList(
      div(class = "d-flex justify-content-end mb-2",
          downloadButton("download_csv", "Download CSV", class = "btn-outline-primary btn-sm")),
      DTOutput("schedule_table")
    )
  })

  output$schedule_table <- renderDT({
    df <- schedule()
    show <- data.frame(
      Day = df$Day, Date = format(df$Date, "%a %d %b"), Step = df$Step, Activity = df$Activity,
      Seed = round(df$Seed_density, 2),
      Predicted = round(df$Predicted_density, 2),
      `Volume (mL)` = df$Volume_mL, Vessel = df$Vessel, Source = df$Source,
      check.names = FALSE)
    names(show)[5:6] <- c("Seed (10<sup>6</sup>/mL)", "Predicted (10<sup>6</sup>/mL)")
    datatable(show, rownames = FALSE, class = "compact stripe hover", escape = TRUE,
              container = htmltools::withTags(table(thead(tr(lapply(names(show), function(h) th(HTML(h))))))),
              options = list(dom = "t", paging = FALSE, ordering = FALSE,
                             columnDefs = list(list(className = "dt-center", targets = c(0, 4, 5, 6)))))
  })

  output$download_csv <- downloadHandler(
    filename = function() sprintf("seed_train_%s_%s.csv", input$experiment, format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      df <- schedule(); df$Date <- format(df$Date, "%Y-%m-%d")
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$equipment_ui <- renderUI({
    s <- strategies(); id <- selected()
    if (is.null(id)) return(empty_state("boxes-stacked", "Choose a strategy to see the equipment it needs."))
    st <- s[[id]]; df <- schedule()
    vessels <- table(df$Vessel[df$Stage != "Production"])
    row <- function(a, b) div(class = "equip-row", span(a), span(b))
    layout_columns(col_widths = c(6, 6),
      card(card_header(st$name),
           row("Production bioreactors", sprintf("%d x %g L", input$bioreactor_count, input$bioreactor_volume)),
           if (!is.null(st$flasks_needed)) row(sprintf("%g mL flasks", input$flask_max_volume), st$flasks_needed),
           if (!is.null(st$waves_needed)) row("Wave bioreactors", st$waves_needed),
           if (!is.null(st$flask_stages)) row("Flask stages", st$flask_stages)),
      card(card_header("Vessels used in the schedule"),
           lapply(names(vessels), function(v) row(v, vessels[[v]])))
    )
  })

  observeEvent(input$help, {
    showModal(modalDialog(
      title = "How the planner works", easyClose = TRUE, size = "l",
      tags$ol(
        tags$li("Cells required = bioreactors x working volume x seed density."),
        tags$li("Each vessel's yield is projected from the passage density and 72 h growth, 2^(72 / doubling time)."),
        tags$li("Routes that exceed your flask or wave limits are dropped; the rest appear as strategy cards."),
        tags$li("The schedule is built backwards from the bioreactor start: the thaw sits at the target cell age, followed by Day 1, Day 4 and Day 7 steps, an optional backbone culture and inoculum expansion.")
      ),
      p(class = "text-muted small", "All values are generic examples for demonstration.")
    ))
  })
}

shinyApp(ui, server)
