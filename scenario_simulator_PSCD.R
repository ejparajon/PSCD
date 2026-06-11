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

# Default state
DEFAULT_STATE <- base_data$State[1]

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

# Setting category names. Every other reference to a
# category string (CSS, weights vector, plot factor levels) reads from here 
CATEGORY_NAMES <- c("Consumer (C)", "Structure (S)", "Regional Market (M)")

base_indicators <- setdiff(colnames(base_data), "State")
missing_groups <- setdiff(base_indicators, indicator_groups_lookup$Indicator)
if (length(missing_groups) > 0) {
  stop(
    "These base_data indicators have no entry in indicator_groups_lookup: ",
    paste(missing_groups, collapse = ", ")
  )
}

# Calculate indicator counts per category dynamically
indicator_counts <- indicator_groups_lookup %>%
  count(Group) %>%
  deframe()

# Read in theme for plot
source("plot_theming.R", local = TRUE)

# Define a null-coalescing operator for use throughout to set defaults (steps etc.)
# Returns left-hand side (a) if  not NULL, otherwise it returns the right-hand side (b)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Weighted standardized score for a single set of indicator values.
#
# vals    : named numeric vector of indicator values (0-1), names = indicator
# weights : named numeric vector keyed by category name (e.g. CATEGORY_NAMES)
# groups  : data.frame with columns Indicator, Group
#
# Returns a single standardized score on 0-100.
#
weighted_standardized_score <- function(vals, weights, groups) {
  ind_names <- names(vals)
  grp <- groups$Group[match(ind_names, groups$Indicator)]
  w <- weights[grp]                       # weight per indicator, by its group
  present <- !is.na(vals) & !is.na(w)     # indicators that contribute
  denom <- sum(w[present])
  if (denom == 0) return(NA_real_)
  numer <- sum(vals[present] * w[present])
  (numer / denom) * 100
}

# Numeric matrix of indicator values, built once at startup. Rows are states,
# columns are indicators in base_indicators order. The baseline ranking reads
# this with a single matrix-vector product instead of scanning the data frame
# per state. col_groups maps each column to its category so a weight vector can
# be expanded to per-column weights by name lookup.
base_matrix <- as.matrix(base_data[base_indicators])
storage.mode(base_matrix) <- "double"
rownames(base_matrix) <- base_data$State
col_groups <- indicator_groups_lookup$Group[match(base_indicators, indicator_groups_lookup$Indicator)]

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
    selectInput(
      "state",
      NULL,
      choices = base_data$State,
      selected = DEFAULT_STATE
    ),
    
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
                                  tabPanel(
                                    title = g,
                                    tagList(
                                      # setting initial values
                                      lapply(inds, function(ind) {
                                        step_val <- indicator_steps[ind] %||% 0.25
                                        
                                        init_val <- base_data[base_data$State == DEFAULT_STATE, ind, drop = TRUE]
                                        init_val <- as.numeric(init_val)
                                        
                                        if (length(init_val) != 1 || is.na(init_val)) {
                                          init_val <- 0
                                        }
                                        
                                        sliderInput(
                                          inputId = paste0("slider_", make.names(ind)),
                                          label = ind,
                                          min = 0,
                                          max = 1,
                                          value = init_val,
                                          step = step_val,
                                          ticks = TRUE
                                        )
                                      })
                                    )
                                  )
                                }))
               )),
      
      # Category Weight Sliders
      tags$div(class = "weight-slider-container",
               h5("Category Weights", style = "color: #2c3e50;"),
               p("Adjust the relative importance of each category. Each weight is applied to every indicator within that category, and the final score is calculated as the sum of all weighted indicator values divided by the maximum possible weighted score.", 
                 style = "font-size: 0.85rem; margin-bottom: 10px; color: #6c757d;"),
               sliderInput("weight_consumer", 
                           sprintf("Consumer (C): (%d indicators)", indicator_counts["Consumer (C)"]),
                           min = 0, max = 100, value = 100, step = 25,
                           post = "%",
                           ticks = TRUE),
               sliderInput("weight_structure", 
                           sprintf("Structure (S): (%d indicators)", indicator_counts["Structure (S)"]),
                           min = 0, max = 100, value = 100, step = 25,
                           post = "%",
                           ticks = TRUE),
               sliderInput("weight_market", 
                           sprintf("Regional Market (M): (%d indicators)", indicator_counts["Regional Market (M)"]),
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
    tags$p(
      style = "font-size: 0.8rem; color: #6c757d; font-style: italic; margin-top: 4px;"
    ),
    plotOutput("barPlot", height = "550px"),
    hr(),
    h3("Indicator Details", style = "font-weight: 700 !important; color: #2c3e50;"),
    uiOutput("indicatorDetailTabs")
  )
  )
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  groups_lookup <- indicator_groups_lookup %>% select(Indicator, Group)
  
  # Weighted baseline: every state scored from the original base_data values
  # under the current category weights. Used to rank the selected state.
  
  # Computed as a single matrix-vector product: base_matrix (states x
  # indicators) times w (per-indicator weights) gives every state's weighted
  # total at once. 
  weighted_baseline <- reactive({
    weights <- category_weights()
    w <- weights[col_groups]                 # weight per matrix column
    denom <- sum(w)
    if (denom == 0) {
      return(setNames(rep(NA_real_, nrow(base_matrix)), rownames(base_matrix)))
    }
    numer <- as.vector(base_matrix %*% w)    # all states in one product
    setNames((numer / denom) * 100, rownames(base_matrix))
  })
  
  # Helper to get current state's base values as named numeric vector
  base_values <- reactive({
    req(input$state)
    row <- base_data[base_data$State == input$state, , drop = FALSE]
    setNames(as.numeric(row[1, base_indicators]), base_indicators)
  })
  
  # When user changes state, update sliders
  observeEvent(input$state, {
    vals <- base_values()
    for (ind in names(vals)) {
      updateSliderInput(
        session,
        paste0("slider_", make.names(ind)),
        value = vals[[ind]]
      )
    }
  }, ignoreInit = TRUE)
  
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
  
  # Debounced copy of indicators(). The plot and score read this so a slider
  # drag triggers one render after the slider settles (250ms) rather than on
  # every intermediate value. Slider-sync observers still read inputs directly,
  # so state changes update sliders with no delay. Can change this value for smoothness
  indicators_d <- debounce(indicators, 250)
  
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
      <em>Effective weight distribution selected:</em>
    </div>
    <div style='font-size: 0.95rem; font-weight: 500;'>
      Consumer: %.0f%% | Structure: %.0f%% | Regional Market: %.0f%%
    </div>
    <div style='font-size: 0.8rem; margin-top: 4px; font-style: italic;'>
      (Applied to %d, %d, and %d indicators respectively)
    </div>
  </div>",
  pcts[["Consumer (C)"]], pcts[["Structure (S)"]], pcts[["Regional Market (M)"]],
  indicator_counts["Consumer (C)"],
  indicator_counts["Structure (S)"],
  indicator_counts["Regional Market (M)"]
    ))
  })
  
  # Reset weights to 100
  observeEvent(input$reset_weights, {
    updateSliderInput(session, "weight_consumer", value = 100)
    updateSliderInput(session, "weight_structure", value = 100)
    updateSliderInput(session, "weight_market", value = 100)
  })
  
  # Get category weights as named vector, keyed by the shared CATEGORY_NAMES
  category_weights <- reactive({
    setNames(
      c(input$weight_consumer %||% 100,
        input$weight_structure %||% 100,
        input$weight_market %||% 100),
      CATEGORY_NAMES
    )
  })
  
  # Total score and state rank 
  output$totalScore <- renderUI({
    vals <- indicators_d()
    weights <- category_weights()
    req(vals, input$state, weights)
    
    # Check for zero total weight
    if (sum(weights) == 0) {
      return(HTML('<div style="font-size: 18px; color: #dc3545;">
      ⚠️ Total weight cannot be 0. Please adjust weights.
    </div>'))
    }
    
    standardized <- weighted_standardized_score(vals, weights, groups_lookup)
    
    weighted_base_vec <- weighted_baseline() 
    weighted_base_vec[input$state] <- standardized 
    
    state_rank <- rank(-weighted_base_vec, ties.method = "min")[input$state]
    
    # Add effective weight distribution if weights are not all equal
    weight_info <- ""
    if (!all(weights == 100)) {
      total_weight <- sum(weights)
      pcts <- (weights / total_weight) * 100
      weight_info <- sprintf(
        "<br><small style='color: #6c757d;'>Effective weight distribution selected: Consumer: %.0f%% | Structure: %.0f%% | Regional Market: %.0f%%</small>",
        pcts[["Consumer (C)"]], pcts[["Structure (S)"]], pcts[["Regional Market (M)"]]
      )
    }
    
    HTML(sprintf(
      '<div style="font-size: 18px; font-weight: bold; color: #2c3e50;">
      Standardized score (0–100%%) for %s: %.2f%% | Dynamic Rank: %d of %d Southeast states%s
    </div>',
    input$state, 
    standardized, 
    as.integer(state_rank), 
    length(weighted_base_vec),
    weight_info
    ))
  })
  
  # Bar plot
  output$barPlot <- renderPlot({
    vals <- indicators_d()
    req(vals)
    
    df <- data.frame(
      Indicator = names(vals),
      Value = vals,
      stringsAsFactors = FALSE
    ) %>%
      left_join(groups_lookup, by = "Indicator")
    
    df$Label <- sprintf("%.2f", df$Value)
    df$Group <- factor(df$Group, levels = CATEGORY_NAMES)
    
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
    output[[output_name]] <- DT::renderDT({
      df <- subset(indicator_details_df, Group == group_name)
      order_vec <- indicator_groups_lookup$Indicator[indicator_groups_lookup$Group == group_name]
      df <- df[match(order_vec, df$Name), ]
      # removing columns I don't want to show up
      df$Group <- NULL
      
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
    tab_panels <- lapply(CATEGORY_NAMES, function(tab_title) {
      id_suffix <- tolower(gsub(" ", "_", tab_title))
      tabPanel(title = tab_title, render_group_table(tab_title, id_suffix))
    })
    do.call(tabsetPanel, c(id = "details_tabs", tab_panels))
  })
}

# Run the app
shinyApp(ui, server)
