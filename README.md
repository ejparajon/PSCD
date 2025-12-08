# PSCD
Policy simulator tool component for the [Power Sector Competitiveness Dashboard](https://nicholasinstitute.duke.edu/project/power-sector-competitiveness-dashboard).

Repo contains the following:

`scenario_simulator_PSCD.R`
Code for running and formatting the Shiny app in the Power Sector Competitiveness Dashboard.

`indicator_formatting.R`
Functions for cleaning, transforming, and standardizing indicator values and labels. Generates `indicator_data.rds`.

`indicator_data.rds`
Processed indicator information used for the dashboard.

`plot_theming.R`
Global ggplot theme settings for plots used across the dashboard.

`state_indicator_data.csv`
Raw state-level indicator inputs before formatting or processing.

`state_indicator_data.rds`
Cleaned, ready-to-use RDS version of the state indicator data for faster loading.
