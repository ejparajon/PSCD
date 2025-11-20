# Preamble ----------------------------------------------------------------

# Loading required packages
library(shiny)        # For building the interactive web app
library(tidyverse)    # For data manipulation (dplyr, tidyr) and plotting (ggplot2)
library(DT)           # For interactive tables
library(bslib)        # For Bootstrap theming in Shiny
library(shinyWidgets) # For advanced widgets, e.g., discrete sliders

# Load data
base_data <- readRDS("state_indicator_data.rds")
indicator_data <- readRDS("indicator_data.rds")

# Clean column names
colnames(base_data) <- colnames(base_data) %>%
  str_trim() %>%
  str_replace_all("\\s+", " ")

clean_name <- function(x) str_trim(x)
colnames(base_data) <- clean_name(colnames(base_data))

# Extract objects
indicator_steps <- indicator_data$steps
indicator_groups_lookup <- indicator_data$groups
indicator_info <- indicator_data$info
indicator_scoring_details <- indicator_data$scoring

source("indicator_info.R", local = TRUE)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # --- Consolidated CSS ---
  tags$head(
    # Load Roboto font
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
    
    # Apply styles
    tags$style(HTML(sprintf("
      /* --- Global Font and DataTable Text Wrap --- */
      body, h1, h2, h3, h4, h5, h6, label, select, button, .nav, .tab-content, .well, .shiny-input-container {
        font-family: 'Roboto', sans-serif !important;
      }
      .dataTables_wrapper .dataTable td { white-space: normal !important; }
      .dataTables_wrapper { width: 100%%; }

      /* --- Slider Tab Spacing --- */
      #indicatorSliders .nav, #indicatorSliders .nav-tabs { margin-bottom: 10px !important; }
      #indicatorSliders .tab-content { margin-top: 10px !important; }
      #indicatorSliders .shiny-input-container { padding-top: 6px !important; }

      /* Slider Numerical Value Labels */
      .irs-min, .irs-max, .irs-single, .irs-from, .irs-to, .irs-label {
        font-size: 0.95em !important; 
        font-weight: bold !important;
      }

      /* --- Tab Header Coloring --- */
      li a[data-value='Consumer (C)'] { color: %s !important; }
      li a[data-value='Structure (S)'] { color: %s !important; }
      li a[data-value='Regional Market (M)'] { color: %s !important; }

      li.active a[data-value='Consumer (C)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }
      li.active a[data-value='Structure (S)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }
      li.active a[data-value='Market (M)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }

      /* --- Label Text Coloring by Category --- */
      .consumer label { color: %s !important; font-weight: bold; }
      .structure label { color: %s !important; font-weight: bold; }
      .market label { color: %s !important; font-weight: bold; }
    ",
    COLOR_MAP["Consumer (C)"], COLOR_MAP["Structure (S)"], COLOR_MAP["Regional Market (M)"],
    COLOR_MAP["Consumer (C)"], COLOR_MAP["Consumer (C)"],
    COLOR_MAP["Structure (S)"], COLOR_MAP["Structure (S)"],
    COLOR_MAP["Regional Market (M)"], COLOR_MAP["Regional Market (M)"],
    COLOR_MAP["Consumer (C)"], COLOR_MAP["Structure (S)"], COLOR_MAP["Regional Market (M)"]
    )))
  ),
  
  titlePanel("Power Sector Competitiveness Dashboard Simulator"),
  # Instructional text
  
  div(
      style = "font-size: 16px; color: #2c3e50; margin-bottom: 20px; max-width: 800px;",
      HTML("
     <p>
       Use the sliders on the left to explore how each policy indicator affects a state’s overall competitiveness score. 
       Select a state and adjust the sliders to simulate alternative policy designs and see the impact on rankings, scores, and individual indicator values.
     </p>

<p>
       Indicators are grouped into Consumer, Structure, and Regional Market categories. 
       Navigate the tabs to switch between categories, and refer to the tables below for detailed definitions and scoring criteria.
     </p>")),


  sidebarLayout(
    sidebarPanel(
      tags$div("Select a State:", style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"),
      selectInput("state", NULL, choices = base_data$State),
      wellPanel(
        h4("Adjust Indicators by Category", style = "font-weight: bold; color: #2c3e50;"),
        actionButton("reset", "Reset to Current Scoring"),
        hr(),
        uiOutput("indicatorSliders")
      )
    ),
    
    mainPanel(
      htmlOutput("totalScore"),
      plotOutput("barPlot", height = "550px"),
      hr(),
      h3("Indicator Details", style = "font-weight: bold; color: #2c3e50;"),
      uiOutput("indicatorDetailTabs")
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Base indicator values
  base_values <- reactive({
    req(input$state)
    indicators <- setdiff(colnames(base_data), "State")
    row <- base_data[base_data$State == input$state, , drop = FALSE]
    setNames(as.numeric(row[1, indicators]), indicators)
  })
  
  # CSS classes for groups
  get_css_class <- function(group_name) {
    if (grepl("Consumer", group_name)) return("consumer")
    if (grepl("Structure", group_name)) return("structure")
    if (grepl("Regional Market", group_name)) return("market")
    ""
  }
  
  # Render sliders
  output$indicatorSliders <- renderUI({
    vals <- base_values()
    req(vals)
    valid_groups <- unique(indicator_groups_lookup$Group)
    
    group_tabs <- lapply(valid_groups, function(g) {
      inds <- indicator_groups_lookup$Indicator[indicator_groups_lookup$Group == g]
      sliders <- lapply(inds, function(ind) {
        sliderInput(
          inputId = paste0("slider_", make.names(ind)),
          label = ind,
          min = 0,
          max = 1,
          value = vals[[ind]],
          step = indicator_steps[ind] %||% 0.25
        )
      })
      tabPanel(title = g, tagList(sliders))
    })
    
    do.call(tabsetPanel, c(id = "indicator_tabs", group_tabs))
  })
  
  # Current indicator values
  indicators <- reactive({
    vals <- base_values()
    sapply(names(vals), function(ind) {
      input[[paste0("slider_", make.names(ind))]] %||% vals[[ind]]
    }) %>% setNames(names(vals))
  })
  
  # Reset sliders
  reset_sliders <- function() {
    vals <- base_values()
    for (ind in names(vals)) {
      updateSliderInput(session, paste0("slider_", make.names(ind)), value = vals[[ind]])
    }
  }
  observeEvent(input$reset, { reset_sliders() })
  observeEvent(input$state, { reset_sliders() })
  
  # Total score
  output$totalScore <- renderText({
    vals <- indicators()
    req(vals)
    total <- sum(vals)
    standardized <- (total / length(vals)) * 100
    
    all_scores <- base_data %>%
      mutate(Total = rowSums(select(., -State), na.rm = TRUE),
             Standardized = (Total / ncol(select(., -State))) * 100) %>%
      mutate(Standardized = ifelse(State == input$state, standardized, Standardized)) %>%
      arrange(desc(Standardized)) %>%
      mutate(Rank = row_number())
    
    state_rank <- all_scores %>% filter(State == input$state) %>% pull(Rank)
    
    HTML(sprintf(
      '<div style="font-size: 22px; font-weight: bold; color: #2c3e50;">
       Standardized score (0–100%%) for %s: %.2f%% | Dynamic Rank: %d of %d Southeast states
       </div>',
      input$state, standardized, state_rank, nrow(all_scores)
    ))
  })
  
  # Bar plot
  output$barPlot <- renderPlot({
    vals <- indicators()
    req(vals)
    
    df <- data.frame(
      Indicator = names(vals),
      Value = vals,
      stringsAsFactors = FALSE) %>% 
      left_join(indicator_groups_lookup, by = "Indicator")
    
    df$Label <- sprintf("%.2f", df$Value)
    df$Group <- factor(df$Group, levels = c("Consumer (C)", "Structure (S)", "Regional Market (M)"))
    
    ggplot(df, aes(x = reorder(Indicator, Value), y = Value, fill = Group)) +
      geom_bar(stat = "identity", color = "black", width = 0.8) +
      geom_label(
        data = df,
        aes(x = reorder(Indicator, Value), y = Value, label = Label),
        inherit.aes = FALSE, size = 5.5, label.size = NA, fill = "white", color = "black", hjust = -0.1) +
      ylim(0, 1.05) +
      coord_flip() +
      scale_fill_manual(values = COLOR_MAP, name = "Category") +
      labs(title = paste("Indicator Values for", input$state), x = "", y = "Score (0–1)") +
      custom_indicator_theme
  })
  
  
  # Indicator details
  indicator_details_df <- data.frame(
    Name = names(indicator_info),
    "Short Description" = unlist(indicator_info, use.names = FALSE),
    "Scoring Criteria" = unlist(indicator_scoring_details, use.names = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>% merge(indicator_groups_lookup, by.x = "Name", by.y = "Indicator")
  
  render_group_table <- function(group_name, id_suffix) {
    output_name <- paste0("detailsTable_", id_suffix)
    output[[output_name]] <- renderDataTable({
      df <- subset(indicator_details_df, Group == group_name)
      df <- df[match(indicator_groups_lookup$Indicator[indicator_groups_lookup$Group == group_name], df$Name), ]
      df$Group <- NULL
      datatable(df, escape = FALSE, rownames = FALSE,
                options = list(pageLength = 10, autoWidth = FALSE, scrollX = TRUE,
                               dom = "t", paging = FALSE, ordering = FALSE,
                               columnDefs = list(
                                 list(width = '19%', targets = 0),
                                 list(width = '38%', targets = 1),
                                 list(width = '43%', targets = 2)
                               ))) %>%
        formatStyle(columns = names(df), whiteSpace = "normal", lineHeight = "1.3em")
    })
    dataTableOutput(output_name)
  }
  
  output$indicatorDetailTabs <- renderUI({
    valid_groups_map <- list(
      "Consumer (C)" = "Consumer (C)",
      "Structure (S)" = "Structure (S)",
      "Regional Market (M)" = "Regional Market (M)"
    )
    tab_panels <- lapply(names(valid_groups_map), function(tab_title) {
      group_full_name <- valid_groups_map[[tab_title]]
      id_suffix <- tolower(gsub(" ", "_", tab_title))
      tabPanel(title = tab_title, render_group_table(group_full_name, id_suffix))
    })
    do.call(tabsetPanel, c(id = "details_tabs", tab_panels))
  })
}

# Run the app -------------------------------------------------------------
shinyApp(ui, server)

