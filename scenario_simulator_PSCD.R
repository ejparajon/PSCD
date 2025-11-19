# Preamble ----------------------------------------------------------------

# Installing and loading packages for use
if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny, tidyverse, DT,bslib)

# Loading in the data file
base_data <- readRDS("state_indicator_data.rds")

# cleaning up names just in case

# Replace all non-breaking spaces with normal spaces
colnames(base_data) <- str_replace_all(colnames(base_data), "\\s+", " ")
# trim whitespace
colnames(base_data) <- str_trim(colnames(base_data))

# Loading in indicator information, color theming and ggplot theme
source("indicator_info.R", local = TRUE)

# Setting up the UI ----------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"), #using bootstrap version 5
  # CONSOLIDATED CSS
  tags$head(
    # Load Roboto font via link
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
    
    # Apply Roboto globally and keep existing styles
    tags$style(HTML(sprintf("
        /* --- 1. Global Font and DataTable Text Wrap (Static) --- */
        body, h1, h2, h3, h4, h5, h6, label, select, button, .nav, .tab-content, .well, .shiny-input-container {
            font-family: 'Roboto', sans-serif !important;
        }
        /* Forces text to wrap in the DataTables cells and ensures 100%% width */
        .dataTables_wrapper .dataTable td {
            white-space: normal !important;
        }
        .dataTables_wrapper {
            width: 100%%;
        }
        
  /* 1) Add space under the tab headers (nav) inside the indicatorSliders output */
  #indicatorSliders .nav,
  #indicatorSliders .nav-tabs {
    margin-bottom: 10px !important;
  }

  /* 2) Add space above the tab-content area in case nav has no margin */
  #indicatorSliders .tab-content {
    margin-top: 10px !important;
  }

  /* 3) If sliders themselves are inside a container (e.g. .shiny-input-container), add a little top padding */
  #indicatorSliders .shiny-input-container {
    padding-top: 6px !important;
  }

        
        /* --- 2. Dynamic Color Styling --- */
        /* General Tab Header Coloring */
        li a[data-value='Consumer (C)'] { color: %s !important; }
        li a[data-value='Structure (S)'] { color: %s !important; }
        li a[data-value='Regional Market (M)'] { color: %s !important; }

        /* Active Tab Styling */
        li.active a[data-value='Consumer (C)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }
        li.active a[data-value='Structure (S)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }
        li.active a[data-value='Market (M)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }

        /* Slider Bar Color */
        .consumer .irs-bar, .consumer .irs-bar-edge { background: %s !important; border-color: %s !important; }
        .Structure .irs-bar, .Structure .irs-bar-edge { background: %s !important; border-color: %s !important; }
        .market .irs-bar, .market .irs-bar-edge { background: %s !important; border-color: %s !important; }

        /* Slider Handle Color */
        .consumer .irs-handle { border-color: %s !important; }
        .Structure .irs-handle { border-color: %s !important; }
        .market .irs-handle { border-color: %s !important; }
            
        /* Slider Label Text Color */
        .consumer label { color: %s !important; font-weight: bold; }
        .Structure label { color: %s !important; font-weight: bold; }
        .market label { color: %s !important; font-weight: bold; }
        ",
        COLOR_MAP["Consumer (C)"], COLOR_MAP["Structure (S)"], COLOR_MAP["Regional Market (M)"],
        COLOR_MAP["Consumer (C)"], COLOR_MAP["Consumer (C)"],
        COLOR_MAP["Structure (S)"], COLOR_MAP["Structure (S)"],
        COLOR_MAP["Regional Market (M)"], COLOR_MAP["Regional Market (M)"],
        COLOR_MAP["Consumer (C)"], COLOR_MAP["Consumer (C)"],
        COLOR_MAP["Structure (S)"], COLOR_MAP["Structure (S)"],
        COLOR_MAP["Regional Market (M)"], COLOR_MAP["Regional Market (M)"],
        COLOR_MAP["Consumer (C)"], COLOR_MAP["Structure (S)"], COLOR_MAP["Regional Market (M)"],
        COLOR_MAP["Consumer (C)"], COLOR_MAP["Structure (S)"], COLOR_MAP["Regional Market (M)"]
    )))
  ),
  
  titlePanel("Power Sector Competitiveness Dashboard Simulator"),
  
  # Instructional text
  div(
    style = "font-size: 16px; color: #2c3e50; margin-bottom: 20px; max-width: 800px;",
    HTML("
    <p>
      Use the sliders on the left to explore how each policy indicator contributes to a state’s 
      overall competitiveness score. Select a state and then adjust the sliders to simulate alternative policy designs 
      and view their effects on rankings, scores, and individual indicator values.
    </p>
    <p>
      Indicators are grouped into Consumer, Structure, and Regional Market categories.  
      Use the tabs to navigate across categories, and review the tables below 
      for detailed definitions and scoring criteria.
    </p>
  ")
  ),
  
  sidebarLayout(
    # State selection
    sidebarPanel(
      tags$div(
        "Select a State:",
        style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"
      ),
      selectInput("state", NULL, choices = base_data$State), # label set to NULL
      
      # Indicator sliders panel
      wellPanel(
        h4("Adjust Indicators by Category", style = "font-weight: bold; color: #2c3e50;"),
        actionButton("reset", "Reset to Current Scoring"),
        hr(style = "border-top: 1px solid #ccc;"),
        uiOutput("indicatorSliders")
      )
    ),
    
    mainPanel(
      htmlOutput("totalScore"),
      plotOutput("barPlot", height = "550px"),
      hr(),
      h3("Indicator Details", style = "font-weight: bold; color: #2c3e50;"),
      uiOutput("indicatorDetailTabs") # UI output for tabbed table layout
    )
  )
)

# Functionality ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive base values for selected state
  base_values <- reactive({
    req(input$state)
    row <- base_data[base_data$State == input$state, , drop = FALSE]
    indicators <- setdiff(names(row), "State")
    setNames(as.numeric(row[1, indicators]), indicators)
  })
  
  # Helper function to map group name to CSS class name
  get_css_class <- function(group_name) {
    if (grepl("Consumer", group_name)) return("consumer")
    if (grepl("Structure", group_name)) return("Structure")
    if (grepl("Regional Market", group_name)) return("market")
    return("") # default
  }
  
  # Render sliders grouped by category
  output$indicatorSliders <- renderUI({
    req(base_values())
    vals <- base_values()
    valid_groups <- unique(indicator_groups_lookup$Group)
    
    group_tabs <- lapply(valid_groups, function(g) {
      inds <- indicator_groups_lookup$Indicator[indicator_groups_lookup$Group == g]
      css_class <- get_css_class(g)
      
      slider_list <- lapply(inds, function(ind) {
        step_val <- indicator_steps[ind] %||% 0.25
        current_val <- vals[[ind]]
        slider_id <- paste0("slider_", make.names(ind))
        
        tags$div(
          class = css_class,
          sliderInput(
            inputId = slider_id,
            label = ind,
            min = 0, max = 1,
            value = current_val,
            step = step_val
          )
        )
      })
      tabPanel(title = g, tagList(slider_list))
    })
    
    do.call(tabsetPanel, c(id = "indicator_tabs", group_tabs))
  })
  
  # Indicator values based on user input (or base values)
  indicators <- reactive({
    vals <- base_values()
    new_vals <- sapply(names(vals), function(ind) {
      input[[paste0("slider_", make.names(ind))]] %||% vals[[ind]]
    })
    setNames(new_vals, names(vals))
  })
  
  # Reset logic
  reset_sliders <- function() {
    vals <- base_values()
    for (ind in names(vals)) {
      updateSliderInput(session, paste0("slider_", make.names(ind)), value = vals[[ind]])
    }
  }
  
  observeEvent(input$reset, { reset_sliders() })
  observeEvent(input$state, { reset_sliders() })
  
  # Display total score  
  output$totalScore <- renderText({
    req(indicators(), input$state)
    
    # Get current simulated values for selected state
    vals <- indicators()
    total <- sum(vals)
    standardized <- (total / length(vals)) * 100
    
    # Compute baseline standardized scores for all states (assuming base_data is available)
    all_scores <- base_data %>%
      mutate(
        Total = rowSums(select(., -State), na.rm = TRUE),
        Standardized = (Total / ncol(select(., -State))) * 100
      )
    
    # Replace the selected state's score with the current simulated score
    all_scores <- all_scores %>%
      mutate(
        Standardized = ifelse(State == input$state, standardized, Standardized)
      ) %>%
      arrange(desc(Standardized)) %>%
      mutate(Rank = row_number())
    
    # Get the new dynamic rank
    state_rank <- all_scores %>%
      filter(State == input$state) %>%
      pull(Rank)
    
    # HTML formatting of the text
    HTML(sprintf(
      '<div style="font-size: 22px; font-weight: bold; color: #2c3e50;">
  Standardized score (0–100%%) for %s: %.2f%% | Dynamic Rank: %d of %d Southeast states
 </div>',
 input$state,  standardized, state_rank, nrow(all_scores)
    ))
  })
  
  # Render a Bar plot that is colored by category
  output$barPlot <- renderPlot({
    req(indicators())
    vals <- indicators()
    # Creating df and merging in category data for grouping
    df <- data.frame(
      Indicator = names(vals),
      Value = vals,
      stringsAsFactors = FALSE
    ) %>%
      left_join(indicator_groups_lookup, by = "Indicator") # add Group info
    
    # Explicitly set factor levels for Group to ensure consistent order
    df$Group <- factor(df$Group, 
                       levels = c("Consumer (C)", "Structure (S)", "Regional Market (M)"))
    # Plot
    
    ggplot(df, aes(x = reorder(Indicator, Value), y = Value, fill = Group)) +
      geom_bar(stat = "identity", color = "black", width = 0.8) +
      geom_label( # Adding label text
        data = df,
        aes(x = reorder(Indicator, Value), y = Value, label = round(Value, 2)),
        inherit.aes = FALSE,
        size = 5.5,
        label.size = NA,
        fill = "white",
        color = "black",
        hjust = -0.1
      ) +
      ylim(0, 1.05) +
      coord_flip() +
      scale_fill_manual( # Coloring categories using the global map
        values = COLOR_MAP,
        name = "Category"
      ) +
      labs(
        title = paste("Indicator Values for", input$state),
        x = "",
        y = "Score (0–1)"
      ) +
      custom_indicator_theme    
  })
  
  # Adding in table
  indicator_details_df <- reactive({
    
    data.frame(
      Name = names(indicator_info),
      "Short Description" = unlist(indicator_info, use.names = FALSE),
      
      # Convert scoring text into HTML-safe text for
      "Scoring Criteria" = unlist(indicator_scoring_details, use.names = FALSE),
      
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) %>%
      left_join(indicator_groups_lookup, by = c("Name" = "Indicator")) %>%
      select(Group, Name, `Short Description`, `Scoring Criteria`)
  })
  
  
  # Helper function to render the DT table for a specific group
  render_group_table <- function(group_name, id_suffix) {
    
    output_name <- paste0("detailsTable_", id_suffix)
    
    output[[output_name]] <- renderDataTable({
      
      df <- indicator_details_df() %>%
        filter(Group == group_name) %>%
        select(-Group)  # remove the group column
      
      datatable(
        df,
        escape = FALSE,    # Allow HTML 
        rownames = FALSE,
        options = list(
          pageLength = 10,
          autoWidth = FALSE,
          scrollX = TRUE,
          dom = "t",
          columnDefs = list(
            list(width = '19%', targets = 0),
            list(width = '38%', targets = 1),
            list(width = '43%', targets = 2)
          )
        )
      ) %>%        # Ensure word-wrapping 
        formatStyle(
          columns = names(df),
          whiteSpace = "normal",
          lineHeight = "1.3em"
        )
    })
    
    dataTableOutput(output_name)
  }
  
  
  # 3. Render the UI with the tabs and the table outputs
  output$indicatorDetailTabs <- renderUI({
    
    # Define the group names explicitly for tab creation
    valid_groups_map <- list(
      "Consumer (C)" = "Consumer (C)",
      "Structure (S)" = "Structure (S)",
      "Regional Market (M)" = "Regional Market (M)"
    )
    
    # Create a list of tabPanel elements
    tab_panels <- lapply(names(valid_groups_map), function(tab_title) {
      group_full_name <- valid_groups_map[[tab_title]]
      
      # Create a clean ID suffix for the helper function
      id_suffix <- tolower(gsub(" ", "_", tab_title))
      
      tabPanel(
        title = tab_title,
        render_group_table(group_full_name, id_suffix)
      )
    })
    
    # Create the tabsetPanel structure
    do.call(tabsetPanel, c(id = "details_tabs", tab_panels))
  })
  
}

# Run the app ----------------------------------------------------------------

shinyApp(ui, server)
