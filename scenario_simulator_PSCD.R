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

# Define a null-coalescing operator for use throughout
# Returns left-hand side (a) if  not NULL, otherwise it returns the right-hand side (b)
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
    li.active a[data-value='Regional Market (M)'] {
      background-color: %s !important; color: white !important;
      font-weight: 600; border-color: %s !important;
    }

    /* --- Label Coloring by Category --- */
    .consumer label { color: %s !important; font-weight: 500; }
    .structure label { color: %s !important; font-weight: 500; }
    .market label { color: %s !important; font-weight: 500; }

    /* Weight sliders styling */
    .weight-slider-container {
      background-color: #f8f9fa;
      padding: 12px;
      border-radius: 5px;
      margin-top: 10px;
      border: 1px solid #dee2e6;
    }
    
    .weight-slider-container h5 {
      margin-bottom: 8px;
      font-weight: 600 !important;
    }

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
               )),
      
      # Category Weight Sliders
      tags$div(class = "weight-slider-container",
               h5("Category Weights", style = "color: #2c3e50;"),
               p("Adjust the relative importance of each category. Each weight is applied to every indicator within that category, and the final score is calculated as the sum of all weighted indicator values divided by the maximum possible weighted score.", 
                 style = "font-size: 0.85rem; margin-bottom: 10px; color: #6c757d;"),
               sliderInput("weight_consumer", 
                           "Consumer (C): (5 indicators)",
                           min = 0, max = 100, value = 100, step = 25,
                           post = "%",
                           ticks = TRUE),
               sliderInput("weight_structure", 
                           "Structure (S): (7 indicators)",
                           min = 0, max = 100, value = 100, step = 25,
                           post = "%",
                           ticks = TRUE),
               sliderInput("weight_market", 
                           "Regional Market (M): (3 indicators)",
                           min = 0, max = 100, value = 100, step = 25,
                           post = "%",
                           ticks = TRUE),
               uiOutput("weight_distribution"),
               actionButton("reset_weights", "Reset Weights to 100%",
                            style = "margin-top: 5px; font-size: 0.85rem;")
      )
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
  # Prepare consistent input IDs & precomputations (run once) 
  indicator_groups_lookup <- indicator_groups_lookup %>%
    mutate(safe_id = make.names(Indicator))
  
  # Precompute baseline totals/standardized for ranking
  baseline_scores_df <- base_data %>%
    mutate(Total = rowSums(select(., -State), na.rm = TRUE),
           Standardized = (Total / (ncol(select(., -State)))) * 100) %>%
    select(State, Standardized)
  
  baseline_vec <- setNames(baseline_scores_df$Standardized, baseline_scores_df$State)
  
  # Weighted baseline 
  weighted_baseline <- reactive({
    weights <- category_weights()
    groups_lookup <- indicator_groups_lookup %>% select(Indicator, Group)
    
    # Calculate max possible weighted score (sum of weights applied to all indicators)
    all_groups <- groups_lookup$Group
    max_weighted_score <- sum(weights[all_groups])
    
    weighted_df <- base_data %>%
      pivot_longer(cols = -State, names_to = "Indicator", values_to = "Value") %>%
      left_join(groups_lookup, by = "Indicator") %>%
      mutate(Weight = weights[Group]) %>%  # Look up weight for each indicator
      mutate(Weighted_Value = Value * Weight) %>%  # Weight each indicator individually
      
      group_by(State) %>%
      summarise(
        Weighted_Total = sum(Weighted_Value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(Standardized = (Weighted_Total / max_weighted_score) * 100)
    
    # Return as named vector for ranking
    setNames(weighted_df$Standardized, weighted_df$State)
  })
  
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
      if (!is.null(input[[input_id]])) {
        updateSliderInput(session, input_id, value = vals[[ind]])
      } else {
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
  
  # Normalized weight distribution (0-100)
  output$weight_distribution <- renderUI({
    weights <- category_weights()
    total <- sum(weights)
    
    if (total == 0) {
      return(HTML("<small style='color: #dc3545; font-size: 0.95rem;'>⚠️ Total weight cannot be 0</small>"))
    }
    
    # Calculate what % each weight actually represents
    pcts <- (weights / total) * 100
    
    HTML(sprintf(
      "<div style='color: #6c757d; margin-top: 8px; margin-bottom: 8px;'>
      <div style='font-size: 0.88rem; margin-bottom: 4px;'>
        <em>Effective weight distribution:</em>
      </div>
      <div style='font-size: 0.95rem; font-weight: 500;'>
        Consumer: %.0f%% | Structure: %.0f%% | Regional Market: %.0f%%
      </div>
      <div style='font-size: 0.8rem; margin-top: 4px; font-style: italic;'>
        (Applied to 5, 7, and 3 indicators respectively)
      </div>
    </div>",
    pcts[1], pcts[2], pcts[3]
    ))
  })
  # Reset weights to 100
  observeEvent(input$reset_weights, {
    updateSliderInput(session, "weight_consumer", value = 100)
    updateSliderInput(session, "weight_structure", value = 100)
    updateSliderInput(session, "weight_market", value = 100)
  })
  
  # Get category weights as named vector
  category_weights <- reactive({
    c("Consumer (C)" = input$weight_consumer %||% 100,
      "Structure (S)" = input$weight_structure %||% 100,
      "Regional Market (M)" = input$weight_market %||% 100)
  })
  
  # Total score and state rank 
  output$totalScore <- renderUI({
    vals <- indicators()
    weights <- category_weights()
    req(vals, input$state, weights)
    
    # Check for zero total weight
    if (sum(weights) == 0) {
      return(HTML('<div style="font-size: 18px; color: #dc3545;">
      ⚠️ Total weight cannot be 0. Please adjust weights.
    </div>'))
    }
    
    # Apply weights to each indicator based on its category
    # This multiplies each indicator's value by its category weight
    # e.g., if Consumer weight is 100 and indicator value is 0.5, weighted value = 50
    weighted_vals <- sapply(names(vals), function(ind) {
      group <- indicator_groups_lookup$Group[indicator_groups_lookup$Indicator == ind]
      vals[[ind]] * weights[[group]]
    })
    total <- sum(weighted_vals, na.rm = TRUE)
    
    # Calculate max possible weighted score
    # This is the sum of category weights applied to each indicator
    # e.g., with C=100, S=100, M=25: (5*100) + (7*100) + (3*25) = 1275
    all_groups <- indicator_groups_lookup$Group
    max_weighted <- sum(weights[all_groups])
    standardized <- (total / max_weighted) * 100 
    
    weighted_base_vec <- weighted_baseline() 
    weighted_base_vec[input$state] <- standardized 
    
    state_rank <- rank(-weighted_base_vec, ties.method = "min")[input$state]
    
    HTML(sprintf(
      '<div style="font-size: 18px; font-weight: bold; color: #2c3e50;">
      Standardized score (0–100%%) for %s: %.2f%% | Dynamic Rank: %d of %d Southeast states
    </div>',
    input$state, 
    standardized, 
    as.integer(state_rank), 
    length(weighted_base_vec)
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
    output[[output_name]] <- renderDT({
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