# Preamble ----------------------------------------------------------------

# Loading required packages
library(shiny)        
library(tidyverse)    
library(DT)           
library(bslib)        
library(scales)      

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

source("plot_theming.R", local = TRUE)

# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # CSS
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    tags$style(HTML(
      sprintf(
        "
    /* --- Global Font --- */
    body, h1, h2, h3, h4, h5, h6, label, select, button,
    .nav, .tab-content, .well, .shiny-input-container {
      font-family: 'Roboto', sans-serif !important;
      font-size: 0.92rem !important;
      font-weight: 400 !important;
      color: #2c3e50;
    }

    /* DataTable wrap */
    .dataTables_wrapper .dataTable td { white-space: normal !important; }
    .dataTables_wrapper { width: 100%%; }

    /* Reduce DataTables font size slightly */
      .dataTables_wrapper .dataTable td,
      .dataTables_wrapper .dataTable th {
        font-size: 0.85rem !important;}

    /* --- Slider Spacing --- */
    #indicatorSliders .nav, #indicatorSliders .nav-tabs { margin-bottom: 8px !important; }
    #indicatorSliders .tab-content { margin-top: 8px !important; }
    #indicatorSliders .shiny-input-container { padding-top: 4px !important; }

    /* Slider Labels */
    .irs-min, .irs-max, .irs-single, .irs-from, .irs-to, .irs-label {
      font-size: 0.85rem !important;
      font-weight: 500 !important;
    }

    /* --- Tab Header Text Colors --- */
    li a[data-value='Consumer (C)']        { color: %s !important; font-weight: 500; }
    li a[data-value='Structure (S)']       { color: %s !important; font-weight: 500; }
    li a[data-value='Regional Market (M)'] { color: %s !important; font-weight: 500; }

    /* --- Active Tabs --- */
    li.active a[data-value='Consumer (C)'] {
      background-color: %s !important; color: white !important;
      font-weight: 600; border-color: %s !important;
    }
    li.active a[data-value='Structure (S)'] {
      background-color: %s !important; color: white !important;
      font-weight: 600; border-color: %s !important;
    }
    li.active a[data-value='Market (M)'] {
      background-color: %s !important; color: white !important;
      font-weight: 600; border-color: %s !important;
    }

    /* --- Label Coloring by Category --- */
    .consumer label { color: %s !important; font-weight: 500; }
    .structure label { color: %s !important; font-weight: 500; }
    .market label { color: %s !important; font-weight: 500; }

  ",
  COLOR_MAP["Consumer (C)"],
  COLOR_MAP["Structure (S)"],
  COLOR_MAP["Regional Market (M)"],
  COLOR_MAP["Consumer (C)"],
  COLOR_MAP["Consumer (C)"],
  COLOR_MAP["Structure (S)"],
  COLOR_MAP["Structure (S)"],
  COLOR_MAP["Regional Market (M)"],
  COLOR_MAP["Regional Market (M)"],
  COLOR_MAP["Consumer (C)"],
  COLOR_MAP["Structure (S)"],
  COLOR_MAP["Regional Market (M)"]
      )
    ))
  ),
  sidebarLayout(sidebarPanel(
    tags$div("Select a State:", style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"),
    selectInput("state", NULL, choices = base_data$State),
    
    wellPanel(
      h4("Adjust Indicators by Category",
         style = "font-weight: 700 !important; color: #2c3e50;"),
      actionButton("reset", "Reset to Original Scoring"),
      hr(),
      # tabsetPanel of sliders
      tags$div(id = "indicatorSliders",
               do.call(
                 tabsetPanel, c(id = "indicator_tabs",
                                lapply(unique(indicator_groups_lookup$Group), function(g) {
                                  inds <-
                                    indicator_groups_lookup$Indicator[indicator_groups_lookup$Group == g]
                                  # inside each tab, create sliders for the indicators
                                  tabPanel(title = g,
                                           tagList(lapply(inds, function(ind) {
                                             step_val <- indicator_steps[ind] %||% 0.25
                                             sliderInput(
                                               inputId = paste0("slider_", make.names(ind)),
                                               label = ind,
                                               min = 0,
                                               max = 1,
                                               value = 0,
                                               step = step_val,
                                               ticks = TRUE
                                             )
                                           })))
                                }))
               ))
    )
  ), 
    # Layout of app
    mainPanel(
      htmlOutput("totalScore"),
      plotOutput("barPlot", height = "550px"),
      hr(),
      h3("Indicator Details", style = "font-weight: 700 !important; color: #2c3e50;"),
      uiOutput("indicatorDetailTabs")
    )
  )
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Define a null-coalescing operator that returns its left-hand side (a) if it's not NULL, otherwise it returns the right-hand side (b)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  # Prepare stable IDs (consistent input IDs) & precomputations (run once) 
  indicator_groups_lookup <- indicator_groups_lookup %>%
    mutate(safe_id = make.names(Indicator))
  
  # Precompute baseline totals/standardized for ranking (global)
  baseline_scores_df <- base_data %>%
    mutate(Total = rowSums(select(., -State), na.rm = TRUE),
           Standardized = (Total / (ncol(select(., -State)))) * 100) %>%
    select(State, Standardized)
  
  baseline_vec <- setNames(baseline_scores_df$Standardized, baseline_scores_df$State)
  
  # Helper to get current state's base values as named numeric vector
  base_values <- reactive({
    req(input$state)
    indicators <- setdiff(colnames(base_data), "State")
    row <- base_data[base_data$State == input$state, , drop = FALSE]
    setNames(as.numeric(row[1, indicators]), indicators)
  })
  
  # When user changes state, update sliders
  observeEvent(input$state, {
    vals <- base_values()
    for (ind in names(vals)) {
      input_id <- paste0("slider_", make.names(ind))
      # only update if this input already exists in input
      if (!is.null(input[[input_id]])) {
        updateSliderInput(session, input_id, value = vals[[ind]])
      } else {
        # If the input doesn't exist yet, skip silently
        next
      }
    }
  }, ignoreInit = FALSE)
  
  # Current indicator values
  indicators <- reactive({
    vals <- base_values()
    out <- sapply(names(vals), function(ind) {
      input_val <- input[[paste0("slider_", make.names(ind))]]
      if (is.null(input_val)) {
        vals[[ind]]
      } else {
        as.numeric(input_val)
      }
    }, simplify = TRUE, USE.NAMES = TRUE)
    setNames(as.numeric(out), names(vals))
  })
  
  # Reset sliders to base values
  reset_sliders <- function() {
    vals <- base_values()
    for (ind in names(vals)) {
      input_id <- paste0("slider_", make.names(ind))
      if (!is.null(input[[input_id]])) {
        updateSliderInput(session, input_id, value = vals[[ind]])
      }
    }
  }
  observeEvent(input$reset, { reset_sliders() })
  
  # Total score and state rank
  output$totalScore <- renderText({
    vals <- indicators()
    req(vals, input$state)
    
    total <- sum(vals, na.rm = TRUE)
    standardized <- (total / length(vals)) * 100
    
    # compute dynamic rank relative to baseline
    baseline_vec2 <- baseline_vec
    baseline_vec2[input$state] <- standardized
    state_rank <- rank(-baseline_vec2)[input$state]
    # This controls text at top
    HTML(sprintf(
      '<div style="font-size: 18px; font-weight: bold; color: #2c3e50;">
      Standardized score (0–100%%) for %s: %.2f%% | Dynamic Rank: %d of %d Southeast states
    </div>',
    input$state, 
    standardized, 
    as.integer(state_rank), 
    length(baseline_vec2)
    ))
  })
  
  # Bar plot
  output$barPlot <- renderPlot({
    vals <- indicators()
    req(vals)
    
    df <- data.frame(
      Indicator = names(vals),
      Value = vals,
      stringsAsFactors = FALSE
    ) %>%
      left_join(indicator_groups_lookup %>% select(Indicator, Group), by = "Indicator")
    
    df$Label <- sprintf("%.2f", df$Value)
    df$Group <- factor(df$Group, levels = c("Consumer (C)", "Structure (S)", "Regional Market (M)"))
    
    ggplot(df, aes(x = reorder(Indicator, Value), y = Value, fill = Group)) +
      geom_col(color = "black", width = 0.58) +       
      geom_text(aes(label = Label),
                hjust = -0.05,
                vjust = 0.5, 
                size = 4.5,
                position = position_stack(vjust = 0.5)) +
      ylim(0, 1.02) +
      coord_flip(clip = "off") +                      
      scale_fill_manual(values = COLOR_MAP, name = "Category") +
      labs(
        title = NULL,                                
        x = NULL, 
        y = "Score: 0 (low) - 1 (high)") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +  
      custom_indicator_theme
  })
  # Details table

  # Indicator details for table
  indicator_details_df <- data.frame(
    Name = names(indicator_info),
    "Short Description" = unlist(indicator_info, use.names = FALSE),
    "Scoring Criteria" = unlist(indicator_scoring_details, use.names = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>%
    merge(indicator_groups_lookup, by.x = "Name", by.y = "Indicator")
  
  # helper to render tables
  render_group_table <- function(group_name, id_suffix) {
    output_name <- paste0("detailsTable_", id_suffix)
    output[[output_name]] <- renderDataTable({
      df <- subset(indicator_details_df, Group == group_name)
      order_vec <- indicator_groups_lookup$Indicator[indicator_groups_lookup$Group == group_name]
      df <- df[match(order_vec, df$Name), ]
      # removing columns I don't want to show up
      df$Group <- NULL
      df$safe_id <- NULL  
      
      # building the table
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

# Run the app
shinyApp(ui, server)
