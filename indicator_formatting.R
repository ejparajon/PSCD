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
  "Third Party PPA (C)" = 0.25,
  "Net Metering (C)" = 0.25,
  "Utility Green Tariffs (C)" = 0.25,
  "Residential Retail Choice (C)" = 0.5,
  "Commercial and Industrial Retail Choice (C)" = 0.5,
  "Plug-in Solar (C)" = 0.25,
  "Virtual Power Plants (C)" = 0.01,
  "Option for Municipal Ownership of Utilities (S)" = 1.0,
  "Presence of a Consumer Advocate (S)" = 1.0,
  "Community Choice Aggregation (S)" = 1.0,
  "Customer Concentration (S)" = 0.5,
  "Generation Concentration (S)" = 0.5,
  "Interconnection Standards (S)" = 0.25,
  "Procurement Requirements (S)" = 0.33,
  "State Authority Role in Wholesale Market (M)" = 0.01,
  "Civil Society Participation Role in Wholesale Market (M)" = 0.01,
  "Market Participation (M)" = 0.2
)



# Adding Category Grouping for the sliders (GLOBAL)
indicator_groups_lookup <- data.frame(
  Indicator = names(indicator_steps),
  Group = c(
    rep("Consumer (C)", 7),
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
  
  "Plug-in Solar (C)" =
    "Evaluates whether state policy enables customers to install small plug-in (balcony) solar devices that connect through a standard household outlet.",
  
  "Virtual Power Plants (C)" =
    "Evaluates whether state policy enables residential customers to enroll battery storage and other distributed energy resources in aggregated programs (virtual power plants) and be compensated for participating.",
  
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
    "Assesses how concentrated each state’s utility generation capacity is by calculating the Herfindahl-Hirschman Index (HHI) from generation capacity shares, following Department of Justice antitrust guidelines.",
  
  "Interconnection Standards (S)" =
    "Measures whether the state has a binding interconnection standard, guidelines, or utility procedures for distributed energy resources, and whether they require the IEEE 1547-2018 technical standard.",
  
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
    "1.0: Permitted by state.<br>0.75: Pilot PPA process available to a majority of utility customers.<br>0.5: Sleeved PPAs permitted and/or pilot programs for less than half of customers.<br>0.25: Partially allowed with notable restrictions (e.g., only solar leases, only tax-exempt entities, or only sleeved PPAs).<br>0.0: Disallowed/restricted OR unknown/unclear.",
  
  "Net Metering (C)" =
    "1.0: State policy enables/requires net metering with no aggregated demand limits.<br>0.75: Net metering allowed with caps >1% of aggregated demand.<br>0.5: Net metering allowed with caps ≤1% of aggregated demand.<br>0.25: No state policy, but utility program(s) exist.<br>0.0: Ban OR no policies/programs.<br>Exception: If limits vary by ownership type, score is the average across categories.",
  
  "Utility Green Tariffs (C)" =
    "1.0: >80% of C&I sales (top 5 largest utilities only) from participating top 5 utilities.<br>0.75: 50–79.9%.<br>0.5: 25–49.9%.<br>0.25: 0.1–24.9%.<br>0.0: No top-5 utilities in the state offer a green tariff program.",
  
  "Residential Retail Choice (C)" =
    "1.0: State actively enables residential retail choice with minimal restrictions.<br>0.5: Allowed with significant limitations (geographic, caps, kWh thresholds).<br>0.0: No enabling policy OR explicit prohibition.",
  
  "Commercial and Industrial Retail Choice (C)" =
    "1.0: State enables large-load customers to choose alternative suppliers with minimal restrictions.<br>0.5: Allowed but with significant limitations (load thresholds, one-time election, geographic limits).<br>0.0: Prohibited OR no programs exist.",
  
  "Plug-in Solar (C)" =
    "1.0: Enacted law provides an interconnection exemption and property/renter protections.<br>0.75: Enacted law provides streamlined interconnection and property/renter protections.<br>0.5: Enacted law provides an interconnection exemption or property/renter protections, but not both.<br>0.25: Enacted law provides streamlined interconnection only.<br>0.0: No enacted enabling law (includes introduced, pending, or failed bills).",
  
  "Virtual Power Plants (C)" =
    "1.0: Qualifying VPP program with broad residential access; third-party aggregators permitted.<br>0.75: Broad access without third-party aggregators, or limited access with third-party aggregators.<br>0.5: Limited access; third-party aggregators not permitted.<br>0.25: Binding action requires a VPP program but no implementation step by December 31, 2027.<br>0.0: No binding qualifying action (includes pending, proposed, or draft actions).<br>Note: Each of the state's five largest utilities (by residential customers) is scored. The state score is the average of those scores, weighted by each utility's share of the five utilities' combined residential customers.",
  
  # --- Structure INDICATORS ---
  "Option for Municipal Ownership of Utilities (S)" =
    "1.0: State policy enables municipalities to create municipal electric authorities.<br>0.0: No enabling policy.",
  
  "Presence of a Consumer Advocate (S)" =
    "1.0: State has a public consumer advocate.<br>0.0: No policy or program in place.",
  
  "Community Choice Aggregation (S)" =
    "1.0: CCA permitted in state policy.<br>0.0: CCA not permitted.",
  
  "Customer Concentration (S)" =
    "1.0: HHI < 1000 (not concentrated).<br>0.5: 1000 ≤ HHI ≤ 1799 (moderately concentrated).<br>0.0: HHI ≥ 1800 (highly concentrated).",
  
  "Generation Concentration (S)" =
    "1.0: HHI < 1000 (not concentrated).<br>0.5: 1000–1799 (moderately concentrated).<br>0.0: ≥1800 (highly concentrated).",
  
  "Interconnection Standards (S)" =
    "1.0: Binding statewide standard (or equivalent) requiring IEEE 1547-2018.<br>0.75: Binding statewide standard with IEEE 1547-2018 adopted by the largest investor-owned utility only; or guidelines or utility procedures with IEEE 1547-2018 required statewide.<br>0.5: Binding statewide standard (or equivalent) without IEEE 1547-2018; or guidelines or utility procedures with IEEE 1547-2018 adopted by the largest investor-owned utility only.<br>0.25: Guidelines or utility procedures without IEEE 1547-2018.<br>0.0: No standard, guidelines, or utility procedures, and no IEEE 1547-2018 adoption.",
  
  "Procurement Requirements (S)" =
    "1.0: All-source solicitations or state has >66% of customers participating in a wholesale market.<br>0.66: Single-source competitive solicitations, or no competitive solicitation requirements but >33% of customers participating in a wholesale market.<br>0.33: No general competitive solicitation requirements, but limited competitive procurement for renewables.<br>0.0: No competitive solicitation requirements of any kind.",
  
  # --- MARKET INDICATORS ---
  "Market Participation (M)" =
    "1.0: ≥90% of customers in RTO/ISO.<br>0.8: 50–89.9%.<br>0.6: 25–49.9%.<br>0.4: SEEM participation + 0.1–24.9% RTO.<br>0.2: SEEM-only, 0% RTO.<br>0.0: No SEEM and no RTO participation.<br>
   Note: A state receives a score based on the RTO/ISO it is in. If a state is in multiple markets, the score is an average of those. If a state is partially in a market(s), but partially not in a market, it will be an average of those.",
  
  "Civil Society Participation Role in Wholesale Market (M)" =
    "1.0: Open membership (MISO, CAISO).<br>
   0.66: Limited voting participation (ISO-NE, SPP).<br>
   0.33: Non-voting participation (PJM).<br>
   0.0: Not in RTO/ISO.<br>
   Note: If a state is partially in a market(s) and with an area not participating in a market, the score is a weighted average of those (proportional to customer counts).",
  
  "State Authority Role in Wholesale Market (M)" =
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

names(indicator_scoring_details) <- str_replace_all(names(indicator_scoring_details), "\\u202F|\\s+", " ")

stopifnot(
  setequal(names(indicator_steps), names(indicator_info)),
  setequal(names(indicator_steps), names(indicator_scoring_details))
)

# Combine everything into a single list
indicator_data <- list(
  steps = indicator_steps,
  groups = indicator_groups_lookup,
  info = indicator_info,
  scoring = indicator_scoring_details
)

# Save to RDS
saveRDS(indicator_data, "indicator_data.rds")
