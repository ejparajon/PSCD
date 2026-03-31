# Power Sector Competitiveness Dashboard Simulator Tool

## Overview
The Policy Simulator is an interactive component of the overall [Power Sector Competitiveness Dashboard](https://nicholasinstitute.duke.edu/project/power-sector-competitiveness-dashboard).
It allows users to explore policy scenarios and evaluate how changes in regulatory structures, market design, and institutional arrangements affect electricity market competitiveness across Southeastern U.S. states (Alabama, Arkansas, Florida, Georgia, Kentucky, Louisiana, Mississippi, North Carolina, South Carolina, Tennessee, Virginia, and West Virginia).

Built using R Shiny, the [Power Sector Competitiveness Dashboard Simulator Tool](https://nicholasinstitute.duke.edu/project/power-sector-competitiveness-dashboard/simulator) translates complex policy changes into quantitative adjustments to the Dashboard’s underlying indicators, enabling transparent and reproducible scenario analysis.
## Functionality
1. Scenario Design: Users can modify State policies, regulatory structures, and market arrangements that influence power sector competitiveness. Outputs are displayed through dynamically updated visualizations. 
2. Indicator Adjustment: The simulator maps policy and structural condition changes to adjustments in the indicators used to construct competitiveness scores. These indicators are normalized (0-1) and aggregated using a consistent methodology across states.
3. Score Recalculation: Once inputs are modified, the simulator recomputes:
   * Individual indicator values
   * Composite competitiveness scores
   * Relative state rankings
4. Weights: Users can optionally adjust the relative importance of the three core competitiveness dimensions:
    * Consumer
    * Structure
    * Regional Market
      
   By default, each category is equally weighted (33% each). All weights are applied dynamically and propagate through the composite scoring framework, updating overall competitiveness scores and state rankings in real time.
## Example use cases
Support research, teaching, and policy evaluation by: 
* Assessing how enhanced consumer options affect competitiveness rankings
* Evaluating the impact of introducing retail competition in a vertically integrated state
* Comparing regional coordination scenarios (e.g., joining an RTO/ISO)

## This repo contains the following files:

`scenario_simulator_PSCD.R`
Code for running and formatting the Shiny app in the Power Sector Competitiveness Dashboard.

`indicator_formatting.R`
Functions for cleaning, transforming, and standardizing indicator values and labels. Generates `indicator_data.rds` and `state_indicator_data.rds`.

`indicator_data.rds`
Processed indicator information used for the dashboard.

`plot_theming.R`
Global ggplot theme settings for plots used across the dashboard.

`state_indicator_data.csv`
Raw state-level indicator inputs before formatting or processing.

`state_indicator_data.rds`
Cleaned, ready-to-use RDS version of the state indicator data for faster loading.
