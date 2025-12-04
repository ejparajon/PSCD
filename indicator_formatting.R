# This code file contains a variety of information used to display the indicators in the sliders, graph, and table
# Define custom step score sizes for each indicator (GLOBAL)
library(tidyverse)
# For re-saving the csv
base_data <- read.csv("state_indicator_data.csv",check.names = FALSE)

# Clean column names just in case
colnames(base_data) <- colnames(base_data) %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()


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
    "Captures whether states permit customers to enter into agreements with third-party developers that install and operate energy systems on the customer’s property and sell power generated to the customer at a contracted rate.",
  
  "Net Metering (C)" = 
    "Measures the extent to which states enable or limit billing systems that provide credits to owners of distributed generation (e.g., solar) that return excess electricity generation to the grid.",
  
  "Utility Green Tariffs (C)" = 
    "Measures the prevalence of large utilities in a state that allow large commercial and industrial customers to buy electricity generated from renewable energy projects at a special rate.",
  
  "Residential Retail Choice (C)" = 
    "Evaluates whether state policy enables residential customers to choose alternative electricity suppliers other than the incumbent utility.",
  
  "Commercial and Industrial Retail Choice (C)" = 
    "Evaluates whether state policy enables commercial and industrial customers to choose alternative electricity suppliers other than the incumbent utility.",
  
  
  # --- Structure INDICATORS ---
  "Option for Municipal Ownership of Utilities (S)" =
    "Indicates whether state policy authorizes municipalities to form their own utility.",
  
  "Presence of a Consumer Advocate (S)" =
    "Evaluates whether states have a consumer advocate office that represents consumer interests in utility proceedings.",
  
  "Community Choice Aggregation (S)" =
    "Measures the presence, or lack thereof, of state policy enabling municipal choice in procuring electricity from a provider other than the incumbent utility on behalf of the area’s population.",
  
  "Customer Concentration (S)" =
    "Assesses how concentrated each state’s utility market overall is by calculating the Herfindahl-Hirschman Index (HHI) from customer counts, following Department of Justice antitrust guidelines.",
  
  "Generation Concentration (S)" =
    "Assesses how concentrated each state’s utility generation capacity is by calculating the Herfindahl-Hirschman Index (HHI) from customer counts and Department of Justice antitrust guidelines.",
  
  "Interconnection Standards (S)" =
    "Measures the presence, or lack thereof, of a state interconnection standard or guideline for investor-owned utilities, or in the absence of a state policy, whether the state’s largest utility has one.",
  
  "Procurement Requirements (S)" =
    "Measures the presence, or lack thereof, of state rules on how utilities may acquire new generation sources. This can include requiring or encouraging competitive procurement for all or some new generation in lieu of defaulting to a default utility-led approach.",
  
  # --- MARKET INDICATORS ---
  "Market Participation (M)" =
    "Evaluates the extent that a state’s utility(ies) participates in a bilateral trading market, such as the Southeast Energy Exchange Market (SEEM), and/or a wholesale electricity market such as a regional transmission organization (RTO)/independent system operator (ISO).",
  
  "Civil Society Participation Role in Wholesale Market (M)" =
    "Evaluates the ability for civil society organizations (e.g. environmental organizations) to participate within the governance structure of a wholesale market (regional transmission organization (RTO) / independent system operator (ISO)), when states that participate in such a market, formalizing multi-actor participation in the process.",
  
  "State Authority Role in Wholesale Market (M)" =
    "Focuses on a state's role within the governance structure of a wholesale market (regional transmission organization (RTO) / independent system operator (ISO)), when states that participate in such a market, formalizing multi-actor participation in the process."
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
   Note: If a state is partially in a market(s) and with an area not participating in a market, the score is a weighted average of those (proportional to customer counts).",
  
  "State Authority Role in Wholesale Regional Market (M)" =
    "1.0: Statutory (CAISO, ERCOT).<br>
   0.66: Delegated (MISO, SPP).<br>
   0.33: Advisory (ISO-NE, NYISO, PJM).<br>
   0.0: No formal role or not in market.<br>
   Note: If a state is partially in a market(s) and with an area not participating in a market, the score is a weighted average of those (proportional to customer counts)."
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
