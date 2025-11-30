# This code file contains a variety of information used to display the indicators in the sliders, graph, and table
#Define custom step score sizes for each indicator (GLOBAL)
library(tidyverse)
# For re-saving the csv
base_data <- read.csv("state_indicator_data.csv",check.names = FALSE)

# Clean column names just in case
colnames(base_data) <- colnames(base_data) %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

# For continuous variables snap them to nearest for purposes of the sliders

# --- Define allowed discrete steps ---
allowed_steps <- c(0, 0.33, 0.66, 1.00)

# --- Helper function to snap any numeric value to the nearest allowed step ---
snap_to_discrete <- function(x, steps = allowed_steps) {
  steps[which.min(abs(steps - x))]
}

# --- List of indicators to snap ---
discrete_indicators <- c(
  "Civil Society Participation Role in Wholesale Market (M)",
  "State Authority Role in Wholesale Market (M)"
)

# --- Apply snapping to base_data ---
for (ind in discrete_indicators) {
  if (ind %in% colnames(base_data)) {
    base_data[[ind]] <- sapply(base_data[[ind]], snap_to_discrete)
  }
}

# Re-saving as a .rds
saveRDS(base_data,"state_indicator_data.rds")

# For re-saving the indicator options
indicator_steps <- c(
  "Third Party PPA (C)" = 0.5,
  "Net Metering (C)" = 0.25,
  "Utility Green Tariffs (C)" = 0.25,
  "Residential Retail Choice (C)" = 0.5,
  "Commercial and Industrial Retail Choice (C)" = 0.5,
  "Option for Municipal Ownership of Utilities (S)" = 1.0,
  "Presence of a Consumer Advocate (S)" = 1.0,
  "Community Choice Aggregation (S)" = 1.0,
  "Customer Concentration (S)" = 0.5,
  "Generation Concentration (S)" = 0.5,
  "Interconnection Standards (S)" = 0.33,
  "Procurement Requirements (S)" = 0.33,
  "State Authority Role in Wholesale Market (M)" = 0.33,
  "Civil Society Participation Role in Wholesale Market (M)" = 0.33,
  "Market Participation (M)" = 0.2
)



# Adding Category Grouping for the sliders (GLOBAL)
indicator_groups_lookup <- data.frame(
  Indicator = names(indicator_steps),
  Group = c(
    rep("Consumer (C)", 5),
    rep("Structure (S)", 7),
    rep("Regional Market (M)", 3)
  ),
  stringsAsFactors = FALSE
)

# Info for table of indicator info
indicator_info <- list(
  
  # --- CONSUMER INDICATORS ---
  "Third Party PPA (C)" = 
    "Power purchase agreements enable consumer choice as an additional finacing mechanism for the adoption of distributed generation (e.g. solar) and purchase of generated electricity from third-party providers.",
  
  "Net Metering (C)" = 
    "Net metering allows owners of distributed generation to send excess electricity back to the grid and receive bill credits or payment.",
  
  "Utility Green Tariffs (C)" = 
    "Green tariffs allow customers—especially C&I users to procure buy bundled renewable energy directly from utilities through a special rate.",
  
  "Residential Retail Choice (C)" = 
    "Indicates whether residential customers may select an alternative electricity supplier instead of the default utility.",
  
  "Commercial and Industrial Retail Choice (C)" = 
    "Indicates whether commercial and industrial customers may choose an alternative electricity supplier instead of the default utility.",
  
  
  # --- Structure INDICATORS ---
  "Option for Municipal Ownership of Utilities (S)" =
    "Indicates whether state policy authorizes municipalities to form their own utility.",
  
  "Presence of a Consumer Advocate (S)" =
    "Indicates whether the state has an independent and statutorily authorized consumer advocate representing ratepayer interests.",
  
  "Community Choice Aggregation (S)" =
    "Indicates whether state policy authorizes local governments to aggregate electricity demand and procure power on behalf of residents.",
  
  "Customer Concentration (S)" =
    "Herfindahl–Hirschman Index (HHI) based on residential customer counts, measuring concentration among utilities.",
  
  "Generation Concentration (S)" =
    "Herfindahl–Hirschman Index (HHI) based on nameplate generating capacity, measuring market concentration among electricity generators.",
  
  "Interconnection Standards (S)" =
    "Measures the presence, or lack thereof, of a state interconnection standard or guideline for investor owned utilities, or in the absence of a state policy, whether the state’s largest utility has one.",
  
  "Procurement Requirements (S)" =
    "Evaluates the presence of competitive procurement practices for new generation, either through state policy and/or through wholesale markets.",
  
  # --- MARKET INDICATORS ---
  "Market Participation (M)" =
    "Measures the extent of state participation in organized wholesale electricity markets (RTO/ISO) or SEEM.",
  
  "Civil Society Participation Role in Wholesale Market (M)" =
    "Measures the degree of formal civil society participation in wholesale market governance structures.",
  
  "State Authority Role in Wholesale Market (M)" =
    "Measures the degree of formal state authority participation in wholesale market governance based on RTO/ISO rules."
)

# --- List of SCORING DETAILS for table---
indicator_scoring_details <- list(
  
  # --- CONSUMER INDICATORS ---
  "Third Party PPA (C)" =
    "1.0: Permitted by state.<br>0.5: Partially permitted (e.g., only solar leases, only tax-exempt entities, or only sleeved PPAs).<br>0.0: Disallowed/restricted OR unknown/unclear.",
  
  "Net Metering (C)" =
    "1.0: State policy enables/requires net metering with no aggregated demand limits.<br>0.75: Net metering allowed with caps >1% of aggregated demand.<br>0.5: Net metering allowed with caps ≤1% of aggregated demand.<br>0.25: No state policy, but utility program(s) exist.<br>0.0: Ban OR no policies/programs.<br>Exception: If limits vary by ownership type, score is the average across categories.",
  
  "Utility Green Tariffs (C)" =
    "1.0: >80% of C&I sales (top 5 largest utilities only) from participating top 5 utilities.<br>0.75: 50–79.9%.<br>0.5: 25–49.9%.<br>0.25: 0.1–24.9%.<br>0.0: No top-5 utilities in the state offer a green tariff program.",
  
  "Residential Retail Choice (C)" =
    "1.0: State actively enables residential retail choice with minimal restrictions.<br>0.5: Allowed with significant limitations (geographic, caps, kWh thresholds).<br>0.0: No enabling policy OR explicit prohibition.",
  
  "Commercial and Industrial Retail Choice (C)" =
    "1.0: State enables large-load customers to choose alternative suppliers with minimal restrictions.<br>0.5: Allowed but with significant limitations (load thresholds, one-time election, geographic limits).<br>0.0: Prohibited OR no programs exist.",
  
  
  # --- Structure INDICATORS ---
  "Option for Municipal Ownership of Utilities (S)" =
    "1.0: State policy enables municipalities to form electric utilities.<br>0.0: No enabling policy.",
  
  "Presence of a Consumer Advocate (S)" =
    "1.0: State has a public consumer advocate.<br>0.0: No consumer advocate.",
  
  "Community Choice Aggregation (S)" =
    "1.0: CCA permitted in state policy.<br>0.0: CCA not permitted.",
  
  "Customer Concentration (S)" =
    "1.0: HHI < 1000 (not concentrated).<br>0.5: 1000 ≤ HHI ≤ 1799 (moderately concentrated).<br>0.0: HHI ≥ 1800 (highly concentrated).",
  
  "Generation Concentration (S)" =
    "1.0: HHI < 1000 (not concentrated).<br>0.5: 1000–1799 (moderately concentrated).<br>0.0: ≥1800 (highly concentrated).",
  
  "Interconnection Standards (S)" =
    "1.0: State Standard with no system size restrictions OR non-IOU largest utility standard with no size limits.<br>0.66: State Standard with system size restriction(s), or if state’s largest utility is non-investor owned utility, the utility’s guideline/policy/procedure with no system size limit.<br>0.33: State Guidelines, or a guideline/policy/procedure from the state’s largest utility if no State Standard/Guideline but largest utility is investor-owned.<br>0.0: No standards/guidelines.",
  
  "Procurement Requirements (S)" =
    "1.0: All-source solicitations or state has >66% of customers participating in a wholesale market.<br>Single-source competitive solicitations or no competitive solicitation requirements but has >33% of customers participating in a wholesale market.<br>0.33: No competitive solicitation requirements (except limited competitive procurement for renewables).<br>0.0: No competitive solicitation requirements.",
  
  # --- MARKET INDICATORS ---
  "Regional Market Participation (M)" =
    "1.0: ≥90% of customers in RTO/ISO.<br>0.8: 50–89.9%.<br>0.6: 25–49.9%.<br>0.4: SEEM participation + 0.1–24.9% RTO.<br>0.2: SEEM-only, 0% RTO.<br>0.0: No SEEM and no RTO participation.",
  
  "Civil Society Participation Role in Wholesale Regional Market (M)" =
    "1.0: Open membership (MISO, CAISO).<br>
   0.66: Limited voting participation (ISO-NE, SPP).<br>
   0.33: Non-voting participation (PJM).<br>
   0.0: Not in RTO/ISO.<br>
   Note (Simulator tool only): Weighted scores across multiple markets are snapped to the nearest discrete value (0.00, 0.33, 0.66, 1.00), so scores may appear differently in the app.",
  
  "State Authority Role in Wholesale Regional Market (M)" =
    "1.0: Statutory (CAISO, ERCOT).<br>
   0.66: Delegated (MISO, SPP).<br>
   0.33: Advisory (ISO-NE, NYISO, PJM).<br>
   0.0: No formal role or not in market.<br>
   Note (Simulator tool only): Weighted scores across multiple markets are snapped to the nearest discrete value (0.00, 0.33, 0.66, 1.00), so scores may appear differently in the app."
)


# Replace non-breaking spaces with regular spaces in all names
names(indicator_steps) <- str_replace_all(names(indicator_steps), "\\u202F|\\s+", " ")
names(indicator_info) <- str_replace_all(names(indicator_info), "\\u202F|\\s+", " ")
indicator_groups_lookup$Indicator <- str_replace_all(indicator_groups_lookup$Indicator, "\\u202F|\\s+", " ")


# Combine everything into a single list
indicator_data <- list(
  steps = indicator_steps,
  groups = indicator_groups_lookup,
  info = indicator_info,
  scoring = indicator_scoring_details
)

# Save to RDS
saveRDS(indicator_data, "indicator_data.rds")
