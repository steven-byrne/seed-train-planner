# Seed Train Planner
# An R Shiny app for planning mammalian cell culture seed trains
# (vial thaw -> shake flask expansion -> optional wave bioreactor -> production bioreactors).
# Example cell lines and media below are generic placeholders.

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(lubridate)
library(rhandsontable)
library(shinyjs)
library(uuid)

# Setup theme
app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#333333",
  primary = "#2C6FB7",
  secondary = "#333333",
  font_scale = 1.0,
  bootswatch = "flatly"
)

ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .card {
        border-radius: 5px;
        background-color: white;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }

      .card-header {
        font-weight: bold;
        border-bottom: 1px solid #eee;
        padding-bottom: 10px;
        margin-bottom: 15px;
      }

      table.dataTable td, table.dataTable th {
        text-align: center !important;
      }

      .split-culture {
        background-color: #f8f8f8;
        border-left: 4px solid #2C6FB7;
        padding: 10px;
        margin-bottom: 10px;
        border-radius: 0 3px 3px 0;
      }

      .notification-area {
        color: #856404;
        background-color: #fff3cd;
        border: 1px solid #ffeeba;
        border-radius: 4px;
        padding: 10px;
        margin-bottom: 15px;
        display: none;
      }

      #timeline {
        height: 300px;
        margin-bottom: 20px;
      }

      .strategy-option {
        background-color: #f8f8f8;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 0 3px 3px 0;
        cursor: pointer;
      }

      .strategy-option.selected {
        background-color: #e8f4f8;
        border-left: 4px solid #2ecc71;
      }

      .callout {
        padding: 15px;
        margin: 10px 0;
        border-radius: 3px;
        border-left: 5px solid #eee;
      }

      .callout-info {
        background-color: #f4f8fa;
        border-left-color: #5bc0de;
      }

      .callout-warning {
        background-color: #fcf8f2;
        border-left-color: #f0ad4e;
      }

      .flex-container {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
      }

      .bioreactor-card {
        background-color: #f8f8f8;
        padding: 10px;
        border-radius: 5px;
        border-left: 4px solid #e74c3c;
        flex: 1 1 200px;
      }

      .backbone-culture {
        background-color: #e8f4fa;
        border-left: 4px solid #3498db;
        padding: 10px;
        border-radius: 3px;
      }
    "))
  ),

  # Dashboard Header
  div(class = "card",
      div(class = "card-header",
          fluidRow(
            column(8, h2("Bioreactor Seed Train Planner", style = "margin-top: 5px; color: #1F4E79;")),
            column(4, align = "right",
                   actionButton("help_button", "Help", icon = icon("question-circle"),
                                class = "btn btn-info", style = "margin-top: 5px;")
            )
          )
      )
  ),

  # Notification area
  div(id = "notification_area", class = "notification-area"),

  fluidRow(
    # Left Column - Bioreactor Configuration
    column(4,
           div(class = "card",
               div(class = "card-header", "Bioreactor Requirements"),

               h4("Production Details"),
               fluidRow(
                 column(6, numericInput("experiment", "Experiment Number:", value = 1001, min = 1, max = 9999, step = 1)),
                 column(6, dateInput("bioreactor_start", "Bioreactor Start:", value = Sys.Date() + 14))
               ),

               selectInput("project", "Cell Line:",
                           choices = c("CHO-K1 Clone A", "CHO-K1 Clone B", "CHO-S Clone C", "CHO-DG44 Clone D", "HEK293 Pool E"),
                           selected = "CHO-K1 Clone A"),

               selectInput("medium", "Medium:",
                           choices = c("Chemically Defined Medium A", "Chemically Defined Medium B", "Passage Medium", "Production Medium"),
                           selected = "Chemically Defined Medium A"),

               hr(),

               h4("Bioreactor Setup"),
               fluidRow(
                 column(6, numericInput("bioreactor_count", "Number of Bioreactors:", value = 4, min = 1, max = 20)),
                 column(6, numericInput("bioreactor_volume", "Volume per Bioreactor (L):", value = 2, min = 1, max = 50))
               ),

               fluidRow(
                 column(6, numericInput("bioreactor_density", "Target Seed Density (×10⁶/mL):", value = 0.5, min = 0.1, max = 2, step = 0.1)),
                 column(6, numericInput("cell_age", "Target Cell Age (days):", value = 21, min = 14, max = 60, step = 1))
               ),

               hr(),

               h4("Growth Parameters"),
               fluidRow(
                 column(6, numericInput("doubling_time", "Doubling Time (hrs):", value = 24, min = 10, max = 40)),
                 column(6, checkboxInput("use_wave", "Use Wave Bioreactor", value = TRUE))
               ),

               conditionalPanel(
                 condition = "input.use_wave == true",
                 fluidRow(
                   column(6, numericInput("wave_max_volume", "Wave Max Volume (mL):", value = 5000, min = 1000, max = 10000)),
                   column(6, numericInput("wave_target_volume", "Wave Target Volume (mL):", value = 4000, min = 1000, max = 10000))
                 )
               ),

               fluidRow(
                 column(6, numericInput("flask_max_volume", "Flask Volume (mL):", value = 250, min = 50, max = 1000)),
                 column(6, numericInput("flask_max_count", "Max Flasks:", value = 6, min = 1, max = 20))
               ),

               hr(),

               fluidRow(
                 column(12, align = "center",
                        actionBttn("calculate_strategies", "Calculate Seed Train Strategies",
                                   style = "fill", color = "primary", size = "md",
                                   icon = icon("calculator"))
                 )
               )
           ),

           # Passage settings
           div(class = "card",
               div(class = "card-header", "Seed Train Passage Settings"),

               fluidRow(
                 column(6, numericInput("vial_cells", "Vial Density (×10⁶/mL):", value = 10.0, step = 0.1)),
                 column(6, numericInput("vial_volume", "Vial Volume (mL):", value = 1, step = 0.1))
               ),

               fluidRow(
                 column(6, numericInput("thaw_density", "Thaw Density (×10⁶/mL):", value = 0.4, step = 0.05)),
                 column(6, numericInput("thaw_volume", "Thaw Volume (mL):", value = 30, step = 1))
               ),

               fluidRow(
                 column(6, numericInput("day1_dilution_threshold", "Day 1 Dilution Threshold (×10⁶/mL):", value = 0.8, step = 0.05)),
                 column(6, numericInput("passage_density", "Passage Density (×10⁶/mL):", value = 0.3, step = 0.05))
               ),

               fluidRow(
                 column(6, numericInput("viability", "Expected Viability (%):", value = 95, min = 50, max = 100)),
                 column(6, checkboxInput("use_backbone", "Use Backbone Culture", value = TRUE))
               )
           )
    ),

    # Right Column - Results and Strategy Options
    column(8,
           div(class = "card",
               div(class = "card-header", "Bioreactor Requirements Summary"),

               fluidRow(
                 column(12, htmlOutput("bioreactor_summary"))
               ),

               hr(),

               h4("Seed Train Timeline", style = "margin-top: 15px;"),
               plotlyOutput("timeline", height = "250px"),

               hr(),

               h4("Available Seed Train Strategies"),
               div(id = "strategy_options",
                   p("Click 'Calculate Seed Train Strategies' to see available options.")
               )
           ),

           div(class = "card",
               div(class = "card-header", "Selected Strategy Details"),

               tabsetPanel(
                 tabPanel("Schedule",
                          br(),
                          div(style = "text-align: right; margin-bottom: 10px;",
                              downloadButton("download_schedule_csv", "Download CSV"),
                              actionButton("edit_schedule", "Edit Schedule", icon = icon("edit"))
                          ),
                          DTOutput("selected_strategy_table")
                 ),
                 tabPanel("Equipment Requirements",
                          br(),
                          htmlOutput("equipment_summary")
                 ),
                 tabPanel("Bioreactors",
                          br(),
                          htmlOutput("bioreactors_detail")
                 ),
                 tabPanel("Backbone Culture",
                          br(),
                          htmlOutput("backbone_summary"),
                          plotlyOutput("backbone_timeline", height = "250px")
                 )
               )
           )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values
  strategies <- reactiveVal(list())
  selected_strategy <- reactiveVal(NULL)
  schedule_data <- reactiveVal(NULL)

  # Show notification
  show_notification <- function(message, type = "warning") {
    shinyjs::removeClass("notification_area", "alert-success alert-danger alert-warning")
    switch(type,
           success = shinyjs::addClass("notification_area", "alert-success"),
           error = shinyjs::addClass("notification_area", "alert-danger"),
           warning = shinyjs::addClass("notification_area", "alert-warning")
    )
    shinyjs::html("notification_area", message)
    shinyjs::show("notification_area")
    if (type == "success") {
      shinyjs::delay(5000, shinyjs::hide("notification_area"))
    }
  }

  # Helper function: determine appropriate vessel
  determine_vessel <- function(volume) {
    if (volume <= 35) {
      return("125 mL flask")
    } else if (volume <= 70) {
      return("250 mL flask")
    } else if (volume <= 170) {
      return("500 mL flask")
    } else if (volume <= 300) {
      return("1L flask")
    } else if (volume <= input$wave_max_volume && input$use_wave) {
      return("Wave Bioreactor")
    } else {
      return("5L Bioreactor")
    }
  }

  # Calculate strategies on button click
  observeEvent(input$calculate_strategies, {
    shinyjs::hide("notification_area")

    # Initial Thawing and Culture Setup (placeholder values)
    thaw_density <- 10 * 10^6
    thaw_volume <- 1
    initial_culture_volume <- 20

    # Calculate total cells needed for all bioreactors
    bioreactor_volume_ml <- input$bioreactor_volume * 1000
    total_bioreactor_volume_ml <- bioreactor_volume_ml * input$bioreactor_count
    cells_per_bioreactor <- input$bioreactor_density * 10^6 * bioreactor_volume_ml
    total_cells_needed <- cells_per_bioreactor * input$bioreactor_count

    # Calculate doubling rates
    doubling_time <- input$doubling_time
    growth_rate_24h <- 2^(24 / doubling_time)
    growth_rate_72h <- 2^(72 / doubling_time)

    # Create different strategy options
    strategy_list <- list()

    # Strategy 1: Use wave bioreactor
    if (input$use_wave) {
      wave_target_volume <- input$wave_target_volume
      wave_seeding_density <- input$passage_density
      cells_from_wave <- wave_target_volume * wave_seeding_density * 10^6 * growth_rate_72h
      bioreactors_per_wave <- floor(cells_from_wave / cells_per_bioreactor)
      if (bioreactors_per_wave >= 1) {
        waves_needed <- ceiling(input$bioreactor_count / bioreactors_per_wave)
        strategy_list$wave <- list(
          name = "Wave Bioreactor Strategy",
          description = sprintf("Use %d wave bioreactor(s) to seed %d production bioreactors",
                                waves_needed, input$bioreactor_count),
          waves_needed = waves_needed,
          bioreactors_per_wave = bioreactors_per_wave,
          wave_volume = wave_target_volume,
          feasible = TRUE
        )
      }
    }

    # Strategy 2: Use only flasks
    flask_volume <- input$flask_max_volume
    flask_seeding_density <- input$passage_density
    cells_from_flask <- flask_volume * flask_seeding_density * 10^6 * growth_rate_72h
    flasks_needed <- ceiling(total_cells_needed / cells_from_flask)
    if (flasks_needed <= input$flask_max_count) {
      strategy_list$flasks <- list(
        name = "Flask Only Strategy",
        description = sprintf("Use %d flask(s) to directly seed %d production bioreactors",
                              flasks_needed, input$bioreactor_count),
        flasks_needed = flasks_needed,
        flask_volume = flask_volume,
        feasible = TRUE
      )
    }

    # Strategy 3: Combination of flasks + wave
    if (input$use_wave && flasks_needed > input$flask_max_count) {
      min_waves_needed <- ceiling(input$bioreactor_count / bioreactors_per_wave)
      cells_needed_for_waves <- min_waves_needed * input$wave_target_volume * input$passage_density * 10^6
      flasks_for_waves <- ceiling(cells_needed_for_waves / cells_from_flask)
      if (flasks_for_waves <= input$flask_max_count) {
        strategy_list$combined <- list(
          name = "Combined Flask + Wave Strategy",
          description = sprintf("Use %d flask(s) to seed %d wave bioreactor(s), then seed %d production bioreactors",
                                flasks_for_waves, min_waves_needed, input$bioreactor_count),
          flasks_needed = flasks_for_waves,
          waves_needed = min_waves_needed,
          bioreactors_per_wave = bioreactors_per_wave,
          flask_volume = flask_volume,
          wave_volume = input$wave_target_volume,
          feasible = TRUE
        )
      }
    }

    # Strategy 4: Multi-stage flask strategy
    if (flasks_needed > input$flask_max_count && (!input$use_wave || length(strategy_list) == 0)) {
      flask_culture_stages <- 1
      current_flasks_needed <- flasks_needed
      while (current_flasks_needed > input$flask_max_count && flask_culture_stages < 5) {
        flask_culture_stages <- flask_culture_stages + 1
        current_flasks_needed <- ceiling(current_flasks_needed / input$flask_max_count)
      }
      if (current_flasks_needed <= input$flask_max_count) {
        strategy_list$multi_flask <- list(
          name = "Multi-Stage Flask Strategy",
          description = sprintf("Use %d stages of flask cultures to seed %d production bioreactors",
                                flask_culture_stages, input$bioreactor_count),
          flask_stages = flask_culture_stages,
          flasks_final_stage = flasks_needed,
          flask_volume = flask_volume,
          feasible = TRUE
        )
      }
    }

    # Update reactive value with strategies
    strategies(strategy_list)

    # Update the UI with strategy options
    update_strategy_options(strategy_list)

    # Update bioreactor summary
    update_bioreactor_summary()

    # Show notification
    if (length(strategy_list) > 0) {
      show_notification(paste(length(strategy_list), "seed train strategies available"), "success")
    } else {
      show_notification("No feasible strategies found with current parameters. Try increasing max flask count or enabling Wave bioreactor.", "error")
    }
  })

  # Update strategy options in UI
  update_strategy_options <- function(strategy_list) {
    if (length(strategy_list) == 0) {
      shinyjs::html("strategy_options", "<p>No feasible strategies found with current parameters.</p>")
      return()
    }
    html_content <- ""
    for (strategy_id in names(strategy_list)) {
      strategy <- strategy_list[[strategy_id]]
      html_content <- paste0(html_content,
                             '<div id="strategy_', strategy_id, '" class="strategy-option" onclick="Shiny.setInputValue(\'select_strategy\', \'', strategy_id, '\');">',
                             '<h5>', strategy$name, '</h5>',
                             '<p>', strategy$description, '</p>',
                             '<button class="btn btn-sm btn-primary select-strategy-btn" data-strategy="', strategy_id, '">Select Strategy</button>',
                             '</div>')
    }
    shinyjs::html("strategy_options", html_content)
  }

  # Handle strategy selection
  observeEvent(input$select_strategy, {
    strategy_id <- input$select_strategy
    strategy_list <- strategies()
    if (!is.null(strategy_id) && strategy_id %in% names(strategy_list)) {
      selected_strategy(strategy_list[[strategy_id]])
      generate_schedule(strategy_id, strategy_list[[strategy_id]])
      for (id in names(strategy_list)) {
        if (id == strategy_id) {
          shinyjs::addClass(paste0("strategy_", id), "selected")
        } else {
          shinyjs::removeClass(paste0("strategy_", id), "selected")
        }
      }
      show_notification(paste("Selected strategy:", strategy_list[[strategy_id]]$name), "success")
    }
  })

  generate_schedule <- function(strategy_id, strategy) {
    schedule <- data.frame()
    doubling_time <- input$doubling_time
    growth_rate_24h <- 2^(24 / doubling_time)
    growth_rate_72h <- 2^(72 / doubling_time)
    bioreactor_date <- input$bioreactor_start
    cell_age <- input$cell_age                       # days from thaw to bioreactor
    passage_density <- input$passage_density * 10^6
    day1_threshold <- input$day1_dilution_threshold * 10^6

    # Define thaw_density and initial_culture_volume (example values)
    thaw_density <- 10 * 10^6
    initial_culture_volume <- 10  # Example value

    # Thaw step
    thaw_date <- bioreactor_date - cell_age
    thaw_row <- data.frame(
      Step = "Thaw",
      Stage = "Initiation",
      Date = format(thaw_date, "%Y-%m-%d (%a)"),
      Activity = "Thaw cell vial",
      Inoculation = thaw_density,
      Predicted_Count = thaw_density * growth_rate_24h,
      Volume_mL = initial_culture_volume,
      Vessel = "Flask",
      Source = "Cell bank",
      Day = -cell_age,
      stringsAsFactors = FALSE
    )
    schedule <- rbind(schedule, thaw_row)

    # Day 1 dilution step
    day1_date <- thaw_date + 1
    if (thaw_density >= day1_threshold) {
      day1_volume <- min(35, (thaw_density / passage_density))
      day1_flask <- determine_vessel(day1_volume)
      day1_row <- data.frame(
        Step = "Day 1 Dilution",
        Stage = "Expansion",
        Date = format(day1_date, "%Y-%m-%d (%a)"),
        Activity = "Dilute culture",
        Inoculation = passage_density,
        Predicted_Count = passage_density * growth_rate_72h,
        Volume_mL = day1_volume,
        Vessel = day1_flask,
        Source = "Thaw",
        Day = -cell_age + 1,
        stringsAsFactors = FALSE
      )
      schedule <- rbind(schedule, day1_row)
    }

    # Day 4 passage step
    day4_date <- day1_date + 3
    day4_volume <- ifelse(thaw_density >= day1_threshold, 100, 50)
    day4_flask <- determine_vessel(day4_volume)
    day4_row <- data.frame(
      Step = "Day 4 Passage",
      Stage = "Expansion",
      Date = format(day4_date, "%Y-%m-%d (%a)"),
      Activity = "Passage culture",
      Inoculation = passage_density,
      Predicted_Count = passage_density * growth_rate_72h,
      Volume_mL = day4_volume,
      Vessel = day4_flask,
      Source = "Day 1 Dilution",
      Day = -cell_age + 4,
      stringsAsFactors = FALSE
    )
    schedule <- rbind(schedule, day4_row)

    # Day 7 passage step
    day7_date <- day4_date + 3
    day7_volume <- 300
    day7_flask <- determine_vessel(day7_volume)
    day7_row <- data.frame(
      Step = "Day 7 Passage",
      Stage = "Expansion",
      Date = format(day7_date, "%Y-%m-%d (%a)"),
      Activity = "Passage culture",
      Inoculation = passage_density,
      Predicted_Count = passage_density * growth_rate_72h,
      Volume_mL = day7_volume,
      Vessel = day7_flask,
      Source = "Day 4 Passage",
      Day = -cell_age + 7,
      stringsAsFactors = FALSE
    )
    schedule <- rbind(schedule, day7_row)

    # Backbone culture step
    backbone_date <- day7_date + 3
    backbone_row <- data.frame(
      Step = "Backbone Culture",
      Stage = "Maintenance",
      Date = format(backbone_date, "%Y-%m-%d (%a)"),
      Activity = "Maintain backbone culture",
      Inoculation = passage_density,
      Predicted_Count = passage_density * growth_rate_72h,
      Volume_mL = 300,
      Vessel = "1L flask",
      Source = "Day 7 Passage",
      Day = -cell_age + 10,
      stringsAsFactors = FALSE
    )
    schedule <- rbind(schedule, backbone_row)

    # Bioreactor inoculum expansion step
    inoculum_date <- bioreactor_date - 6
    inoculum_row <- data.frame(
      Step = "Bioreactor Inoculum",
      Stage = "Expansion",
      Date = format(inoculum_date, "%Y-%m-%d (%a)"),
      Activity = "Expand inoculum",
      Inoculation = input$bioreactor_density * 10^6,
      Predicted_Count = input$bioreactor_density * 10^6 * growth_rate_72h,
      Volume_mL = 300,
      Vessel = "1L flask",
      Source = "Backbone Culture",
      Day = -6,
      stringsAsFactors = FALSE
    )
    schedule <- rbind(schedule, inoculum_row)

    # Optional adjustment step if required
    if (bioreactor_date < (inoculum_date + 6)) {
      adjustment_date <- bioreactor_date - 1
      adjustment_row <- data.frame(
        Step = "Adjustment",
        Stage = "Expansion",
        Date = format(adjustment_date, "%Y-%m-%d (%a)"),
        Activity = "Adjust culture",
        Inoculation = 1 * 10^6,
        Predicted_Count = 1 * 10^6 * growth_rate_24h,
        Volume_mL = 300,
        Vessel = "1L flask",
        Source = "Bioreactor Inoculum",
        Day = -1,
        stringsAsFactors = FALSE
      )
      schedule <- rbind(schedule, adjustment_row)
    }

    # Round volumes for display
    schedule$Volume_mL <- round(schedule$Volume_mL, 1)

    # Save schedule to reactive value
    schedule_data(schedule)

    # Update equipment and details displays
    update_equipment_summary()
    update_bioreactors_detail()

    # Render timeline
    output$timeline <- renderPlotly({
      generate_timeline(schedule)
    })

    # Render schedule table in "Schedule" Tab
    output$selected_strategy_table <- renderDT({
      req(schedule_data())
      datatable(schedule_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
  }

  # Generate timeline visualization using plotly
  generate_timeline <- function(schedule) {
    req(schedule)
    schedule$PlotDate <- as.Date(substr(schedule$Date, 1, 10))
    stages <- unique(schedule$Stage)
    colors <- c("#3498db", "#2ecc71", "#e74c3c")
    names(colors) <- c("Initiation", "Expansion", "Production")
    p <- plot_ly(height = 250) %>%
      layout(
        title = "Seed Train Timeline",
        xaxis = list(title = "Date", tickfont = list(size = 10)),
        yaxis = list(title = "Stage", tickfont = list(size = 10), categoryorder = "array", categoryarray = rev(c("Initiation", "Expansion", "Production"))),
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
        legend = list(orientation = 'h', y = -0.2),
        hovermode = "closest"
      )
    for(stage in stages) {
      stage_data <- schedule[schedule$Stage == stage,]
      p <- p %>% add_trace(
        x = stage_data$PlotDate,
        y = rep(stage, nrow(stage_data)),
        type = 'scatter',
        mode = 'markers',
        name = stage,
        marker = list(size = 12, color = colors[stage]),
        text = paste(
          "Step:", stage_data$Step, "<br>",
          "Date:", format(stage_data$PlotDate, "%b %d (%a)"), "<br>",
          "Activity:", stage_data$Activity, "<br>",
          "Volume:", stage_data$Volume_mL, "mL", "<br>",
          "Vessel:", stage_data$Vessel
        ),
        hoverinfo = 'text'
      )
    }
    p <- p %>% add_trace(
      x = schedule$PlotDate,
      y = schedule$Stage,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'rgba(150,150,150,0.5)', width = 2),
      showlegend = FALSE
    )
    p
  }

  # Update bioreactor summary display
  update_bioreactor_summary <- function() {
    strategy <- selected_strategy()
    if (is.null(strategy)) return()
    summary_text <- paste(
      "Selected Strategy:", strategy$name, "<br>",
      "Description:", strategy$description, "<br>",
      "Total Bioreactors:", input$bioreactor_count, "<br>",
      "Volume per Bioreactor:", input$bioreactor_volume, "L<br>",
      "Total Volume:", input$bioreactor_volume * input$bioreactor_count, "L<br>"
    )
    shinyjs::html("bioreactor_summary", summary_text)
  }

  # Update equipment summary display
  update_equipment_summary <- function() {
    strategy <- selected_strategy()
    if (is.null(strategy)) return()
    equipment_text <- ""
    if (!is.null(strategy$flasks_needed)) {
      equipment_text <- paste(equipment_text, "Flasks Needed:", strategy$flasks_needed, "<br>")
    }
    if (!is.null(strategy$waves_needed)) {
      equipment_text <- paste(equipment_text, "Wave Bioreactors Needed:", strategy$waves_needed, "<br>")
    }
    shinyjs::html("equipment_summary", equipment_text)
  }

  # Update bioreactors detail display
  update_bioreactors_detail <- function() {
    strategy <- selected_strategy()
    if (is.null(strategy)) return()
    detail_text <- ""
    if (!is.null(strategy$flasks_needed)) {
      detail_text <- paste(detail_text, "Flasks Needed:", strategy$flasks_needed, "<br>")
    }
    if (!is.null(strategy$waves_needed)) {
      detail_text <- paste(detail_text, "Wave Bioreactors Needed:", strategy$waves_needed, "<br>")
    }
    shinyjs::html("bioreactors_detail", detail_text)
  }

  # Help button functionality
  observeEvent(input$help_button, {
    showModal(modalDialog(
      title = "Help",
      "This application helps you plan bioreactor seed train strategies. Fill in the required fields and click 'Calculate Seed Train Strategies' to see the available options.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui, server)
