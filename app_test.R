# Preamble ----------------------------------------------------------------

# Installing and loading packages for use
if (!require("pacman")) install.packages("pacman")


pacman::p_load(shiny, tidyverse, shinyjs, shinyBS, DT) # Added DT for interactive table

# Loading in the data file, this assumes the datafile is saved in the same working directory as the R code file.
base_data <- read.csv("state_indicator_data.csv", check.names = FALSE)


# Define custom step score sizes for each indicator (GLOBAL)
indicator_steps <- c(
  "Third Party PPA (C)" = 0.5,
  "Net Metering (C)" = 0.25,
  "Utility Green Tariffs (C)" = 0.25,
  "Residential Choice (C)" = 0.5,
  "Commercial and Industrial Choice (C)" = 0.5,
  "Municipal Ownership of Utilities (S)" = 1.0,
  "Presence of a Consumer Advocate (S)" = 1.0,
  "Customer Concentration (S)" = 0.5,
  "Generation Concentration (S)" = 0.5,
  "Community Choice Aggregation (S)" = 0.25,
  "Procurement Requirements (S)" = 0.33,
  "Interconnection Standards (S)" = 0.25,
  "State Authority Role in Wholesale Market (M)" = 0.33,
  "Civil Society Participation Role in Wholesale Market (M)" = 0.33,
  "Market Participation (M)" = 0.2
)

# Adding Category Grouping for the sliders (GLOBAL)
indicator_groups_lookup <- data.frame(
  Indicator = names(indicator_steps),
  Group = c(
    rep("Consumer (C)", 5),
    rep("Structural (S)", 7),
    rep("Market (M)", 3)
  ),
  stringsAsFactors = FALSE
)


indicator_info <- list(
  "Third Party PPA (C)" = "Third Party Power Purchase Agreements allow non-utility entities to sell renewable energy directly to consumers.",
  "Net Metering (C)" = "Net Metering allows customers to offset their electricity bills by producing their own electricity.",
  "Utility Green Tariffs (C)" = "Green tariffs offered by utilities let customers pay for renewable energy directly.",
  "Residential Choice (C)" = "Residential customers can choose their electricity provider.",
  "Commercial and Industrial Choice (C)" = "Commercial/Industrial customers can choose their electricity provider.",
  "Municipal Ownership of Utilities (S)" = "Local government ownership of electric utilities.",
  "Presence of a Consumer Advocate (S)" = "State office or entity representing consumer interests in utility regulation.",
  "Customer Concentration (S)" = "Degree to which customers are concentrated among a few utilities.",
  "Generation Concentration (S)" = "Degree to which electricity generation is concentrated among a few providers.",
  "Community Choice Aggregation (S)" = "Allows local governments to aggregate electricity demand and purchase energy on behalf of residents.",
  "Procurement Requirements (S)" = "State rules on how utilities must procure renewable energy.",
  "Interconnection Standards (S)" = "Technical standards for connecting distributed energy resources to the grid.",
  "State Authority Role in Wholesale Market (M)" = "Extent to which the state regulates wholesale electricity markets.",
  "Civil Society Participation Role in Wholesale Market (M)" = "Role of civil society organizations in shaping market rules.",
  "Market Participation (M)" = "Degree of market openness and competition in electricity markets."
)

# --- NEW LIST OF PLAUSIBLE SCORING DETAILS ---
indicator_scoring_details <- list(
  # Consumer (C)
  "Third Party PPA (C)" = "1.0: PPAs explicitly allowed, few regulatory hurdles.\n0.5: PPAs allowed but with significant caps or regulatory complexity.\n0.0: PPAs banned or effectively prohibited by law.",
  "Net Metering (C)" = "1.0: Net metering available to all residential and commercial customers at full retail rate.\n0.75: Available at retail rate, but with low capacity caps.\n0.5: Available at an avoided-cost or lower rate.\n0.25: Limited availability or very low compensation rate.\n0.0: Not available.",
  "Utility Green Tariffs (C)" = "1.0: Multiple green tariff options with high subscription rate and low costs.\n0.75: At least one green tariff option available statewide.\n0.5: Tariff available only to large C&I customers.\n0.25: Pilot program only or highly limited availability.\n0.0: No green tariffs offered.",
  "Residential Choice (C)" = "1.0: Residential customers can choose energy supplier.\n0.5: Limited choice (e.g., municipal aggregation only).\n0.0: No choice, mandated bundled service.",
  "Commercial and Industrial Choice (C)" = "1.0: C&I customers can choose energy supplier.\n0.5: Limited choice (e.g., only large load customers).\n0.0: No choice, mandated bundled service.",
  
  # Structural (S)
  "Municipal Ownership of Utilities (S)" = "1.0: High penetration of municipal utilities/co-ops (>= 30% of load).\n0.0: Predominantly investor-owned utilities (< 5% of load).",
  "Presence of a Consumer Advocate (S)" = "1.0: Independent, well-funded state consumer advocate with statutory standing.\n0.0: No dedicated consumer advocate office.",
  "Customer Concentration (S)" = "1.0: High customer fragmentation (top 3 utilities serve < 50% of customers).\n0.5: Moderate concentration.\n0.0: High concentration (top 3 utilities serve > 90% of customers).",
  "Generation Concentration (S)" = "1.0: High generation competition (top 3 generators own < 30% of capacity).\n0.5: Moderate concentration.\n0.0: High concentration (top 3 generators own > 70% of capacity).",
  "Community Choice Aggregation (S)" = "1.0: CCA explicitly authorized and active statewide.\n0.75: Authorized but low participation/limited scope.\n0.5: Authorization process underway or limited pilots.\n0.25: Legislation pending or highly restricted.\n0.0: No legal authorization for CCA.",
  "Procurement Requirements (S)" = "1.0: Strong Renewable Portfolio Standard (RPS) mandate (> 30% by 2030).\n0.67: Moderate RPS or voluntary clean energy goals.\n0.33: Weak RPS or very low goals.\n0.0: No statewide procurement mandate.",
  "Interconnection Standards (S)" = "1.0: Streamlined, transparent interconnection process (national standards applied).\n0.75: Standardized process but slow administrative review.\n0.5: Varies by utility, some significant hurdles.\n0.25: Non-standardized, complex process.\n0.0: No clear, published standards.",
  
  # Market (M)
  "State Authority Role in Wholesale Market (M)" = "1.0: Independent Public Utility Commission (PUC) with strong regulatory capacity in a deregulated market.\n0.67: PUC has jurisdiction but limited scope in wholesale.\n0.33: Weak or politically dependent PUC.\n0.0: Wholesale market fully controlled by vertically integrated utilities.",
  "Civil Society Participation Role in Wholesale Market (M)" = "1.0: Formal participation and funding mechanisms for civil society groups in market/PUC proceedings.\n0.67: Groups can participate but face financial/access hurdles.\n0.33: Ad-hoc or very limited involvement.\n0.0: No formal civil society role.",
  "Market Participation (M)" = "1.0: Fully competitive, integrated regional transmission organization (RTO) market.\n0.8: Partially competitive market (e.g., energy-only).\n0.6: Market exists but with significant barriers to entry.\n0.4: Limited bilateral trading.\n0.2: Vertically integrated monopoly.\n0.0: No open market mechanisms."
)
# --- END NEW LIST ---


# Define the color map for easy reference (GLOBAL)
COLOR_MAP <- c(
  "Consumer (C)" = "#dadaeb",
  "Structural (S)" = "#9e9ac8",
  "Market (M)" = "#54278f"
)

# Setting up the UI ----------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(), # initialize shinyjs for JS tooltip handling
  
  titlePanel("Power Sector Competitiveness Dashboard Simulator"),
  
  # Custom CSS for tab and slider coloring
  tags$head(
    # new font
    tags$style(HTML("
             @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap');
             body { 
                 font-family: 'Roboto', sans-serif; 
             }
         ")),
    # -----------------------------------------------
    tags$style(HTML(sprintf("     /* --- General Tab Header Coloring --- */
       li a[data-value='Consumer (C)'] { color: %s !important; }
       li a[data-value='Structural (S)'] { color: %s !important; }
       li a[data-value='Market (M)'] { color: %s !important; }

       /* --- Active Tab Styling --- */
       li.active a[data-value='Consumer (C)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }
       li.active a[data-value='Structural (S)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }
       li.active a[data-value='Market (M)'] { background-color: %s !important; color: white !important; font-weight: bold; border-color: %s !important; }

       /* --- Slider Bar Color --- */
       .consumer .irs-bar, .consumer .irs-bar-edge { background: %s !important; border-color: %s !important; }
       .structural .irs-bar, .structural .irs-bar-edge { background: %s !important; border-color: %s !important; }
       .market .irs-bar, .market .irs-bar-edge { background: %s !important; border-color: %s !important; }

       /* Slider Handle Color */
       .consumer .irs-handle { border-color: %s !important; }
       .structural .irs-handle { border-color: %s !important; }
       .market .irs-handle { border-color: %s !important; }
        
       /* Slider Label Text Color */
       .consumer label { color: %s !important; font-weight: bold; }
       .structural label { color: %s !important; font-weight: bold; }
       .market label { color: %s !important; font-weight: bold; }
     ",
     COLOR_MAP["Consumer (C)"], COLOR_MAP["Structural (S)"], COLOR_MAP["Market (M)"],
     COLOR_MAP["Consumer (C)"], COLOR_MAP["Consumer (C)"],
     COLOR_MAP["Structural (S)"], COLOR_MAP["Structural (S)"],
     COLOR_MAP["Market (M)"], COLOR_MAP["Market (M)"],
     COLOR_MAP["Consumer (C)"], COLOR_MAP["Consumer (C)"],
     COLOR_MAP["Structural (S)"], COLOR_MAP["Structural (S)"],
     COLOR_MAP["Market (M)"], COLOR_MAP["Market (M)"],
     COLOR_MAP["Consumer (C)"], COLOR_MAP["Structural (S)"], COLOR_MAP["Market (M)"],
     COLOR_MAP["Consumer (C)"], COLOR_MAP["Structural (S)"], COLOR_MAP["Market (M)"]
    )))
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", choices = base_data$State),
      actionButton("reset", "Reset to Current Scoring"),
      uiOutput("indicatorSliders") # slider inputs will be dynamically rendered
    ),
    
    mainPanel(
      htmlOutput("totalScore"),
      plotOutput("barPlot"),
      hr(), # Separator before the new table
      h3("Indicator Details", style = "font-weight: bold; color: #2c3e50;"),
      uiOutput("indicatorDetailTabs") # UI output for the new tabbed table layout
    )
  )
)

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
    if (grepl("Structural", group_name)) return("structural")
    if (grepl("Market", group_name)) return("market")
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
    
    tagList(
      h4("Adjust Indicators by Category"),
      do.call(tabsetPanel, c(id = "indicator_tabs", group_tabs))
    )
  })
  
  
  indicators <- reactive({
    vals <- base_values()
    new_vals <- sapply(names(vals), function(ind) {
      input[[paste0("slider_", make.names(ind))]] %||% vals[[ind]]
    })
    setNames(new_vals, names(vals))
  })
  
  # Reset sliders to base state
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
      '<div style="font-size: 20px; font-weight: bold; color: #2c3e50;">
 Total score for %s: %.2f | Standardized score (0–100%%): %.2f%% | Dynamic Rank: %d of %d Southeast states
 </div>',
 input$state, total, standardized, state_rank, nrow(all_scores)
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
                       levels = c("Consumer (C)", "Structural (S)", "Market (M)"))
    
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
      theme_minimal(base_size = 16) + # Increasing font size
      theme(
        axis.text.y = element_text(size = 16, color = "#2c3e50"),
        axis.text.x = element_text(size = 16, color = "#2c3e50"),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()  
      )      
  })
  
  # 1. Create a master dataframe for indicator details using the new scoring list
  indicator_details_df <- reactive({
    
    data.frame(
      Name = names(indicator_info),
      "Short Description" = unlist(indicator_info, use.names = FALSE),
      # Use the new detailed scoring list here
      "Scoring Criteria" = unlist(indicator_scoring_details, use.names = FALSE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) %>%
      left_join(indicator_groups_lookup, by = c("Name" = "Indicator")) %>%
      # Reorder columns for display
      select(Group, Name, `Short Description`, `Scoring Criteria`)
  })
  
  # Helper function to render the DT table for a specific group
  render_group_table <- function(group_name, id_suffix) {
    # Define a custom output function for the table
    output_name <- paste0("detailsTable_", id_suffix)
    
    output[[output_name]] <- renderDataTable({
      df <- indicator_details_df() %>%
        filter(Group == group_name) %>%
        select(-Group) # Don't display Group in the table body
      
      datatable(df, 
                options = list(
                  pageLength = 10, 
                  autoWidth = TRUE, 
                  dom = 'ftip',
                  columnDefs = list(list(className = 'dt-left', targets = '_all')) # Align text left
                ), 
                rownames = FALSE,
                caption = NULL)
    })
    
    return(dataTableOutput(output_name))
  }
  
  # 3. Render the UI with the tabs and the table outputs
  output$indicatorDetailTabs <- renderUI({
    
    # Define the group names explicitly for tab creation
    valid_groups_map <- list(
      "Consumer (C)" = "Consumer (C)",
      "Structural (S)" = "Structural (S)",
      "Market (M)" = "Market (M)"
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