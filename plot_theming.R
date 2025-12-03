# --- Dashboard Styling ----------------------------------------------------------------------
# Load required packages
library(ggplot2)
library(showtext)
library(sysfonts)
# Load Google Roboto font for plots
showtext_auto()
sysfonts::font_add_google("Roboto", "Roboto")

# Color mapping for indicator categories
COLOR_MAP <- c(
  "Consumer (C)" = "#dadaeb",
  "Structure (S)" = "#9e9ac8",
  "Regional Market (M)" = "#54278f"
)

# --- ggplot Theme ----------------------------------------------------------------------

custom_indicator_theme <- theme_minimal(base_size = 14, base_family = "Roboto") +
  theme(
    # Axis text: slightly smaller for laptop screens 
    axis.text.x = element_text(size = 13, color = "#2c3e50"),
    axis.text.y = element_text(size = 13, color = "#2c3e50"),
    
    # Axis titles: lighter and smaller to reduce vertical space
    axis.title.y = element_text(size = 15, margin = margin(r = 6)),
    axis.title.x = element_text(size = 15, margin = margin(t = 6)),
    
    # Title: centered
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    
    # Legend: 
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13, face = "bold"),
    
    # Reduce left/right padding so long labels fit better
    plot.margin = margin(t = 10, r = 25, b = 10, l = 10),
    
    # Clean up grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
