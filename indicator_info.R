# --------------------------------------------------
# Indicator Configuration and Dashboard Styling
# --------------------------------------------------

# Color mapping for indicator categories
COLOR_MAP <- c(
  "Consumer (C)" = "#dadaeb",
  "Structure (S)" = "#9e9ac8",
  "Regional Market (M)" = "#54278f"
)

# ---------------------------
# Global ggplot Theme
# ---------------------------

# Load required packages
library(ggplot2)
library(showtext)

# Load Google Roboto font for plots
font_add_google("Roboto", "Roboto")
showtext_auto()

# Set global ggplot theme
theme_set(
  theme_minimal(base_size = 16, base_family = "Roboto") +
    theme(
      text = element_text(family = "Roboto"),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(),
      axis.text = element_text(),
      legend.text = element_text(),
      legend.title = element_text()
    )
)

# Custom theme for indicator bar charts
custom_indicator_theme <- theme_minimal(base_size = 12, base_family = "Roboto") +
  theme(
    axis.text.x = element_text(size = 12, color = "#2c3e50"),
    axis.text.y = element_text(size = 12, color = "#2c3e50"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
