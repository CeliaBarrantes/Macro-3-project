library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

##Restructing the 
wb <- read.csv("data/raw/WB_extract_2704.csv", header = TRUE, check.names = FALSE)
View(wb)
wb_long <- wb %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    variable = `Series Name`,
    variable_code = `Series Code`
  ) %>%
  pivot_longer(
    cols = -c(country, country_code, variable, variable_code),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(sub("^(\\d{4}).*", "\\1", year)),
    value = na_if(value, ".."),
    value = as.numeric(gsub(",", "", value))
  )
  View(wb_long)
wb_long <- wb_long %>%
  filter(!is.na(variable), variable != "")
wb_long <- wb_long %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(
    grepl("GDP \\(current US\\$\\)", variable) ~ "GDP_current",
    grepl("GDP per capita \\(current US\\$\\)", variable) ~ "GDP_pc_current",
    grepl("GDP per capita \\(constant 2015 US\\$\\)", variable) ~ "GDP_pc_const",
    grepl("Imports of goods and services", variable) ~ "Imports",
    grepl("Exports of goods and services", variable) ~ "Exports",
    grepl("Current account balance", variable) ~ "CA",
    grepl("Population, total", variable) ~ "Population",
    TRUE ~ variable
  ))
wb_wide <- wb_long %>%
  select(-variable_code) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = mean,       # or first()
    values_fill = NA
  )
View(wb_wide)

data_er <- read.csv("data/raw/Exchange_rate_2704.csv", header = TRUE, check.names = FALSE)
View(data_er)
data_er_long <- data_er %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    variable = `Series Name`,
    variable_code = `Series Code`
  ) %>%
  pivot_longer(
    cols = -c(country, country_code, variable, variable_code),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(sub("^(\\d{4}).*", "\\1", year)),
    value = na_if(value, ".."),
    value = as.numeric(gsub(",", "", value))
  )
View(data_er_long)
data_er_long <- data_er_long %>%
  filter(!is.na(variable), variable != "")
data_er_long <- data_er_long %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(
    grepl("Official exchange rate \\(LCU per US\\$\\, period average)", variable) ~ "Exchange_rate",
    TRUE ~ variable
  ))
data_er_wide <- data_er_long %>%
  select(-variable_code) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = mean,       # or first()
    values_fill = NA
  )
View(data_er_wide)

data_demo <- read.csv("data/raw/Age_depency_2704.csv", header = TRUE, check.names = FALSE)
View(data_demo)
data_demo_long <- data_demo %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    variable = `Series Name`,
    variable_code = `Series Code`
  ) %>%
  pivot_longer(
    cols = -c(country, country_code, variable, variable_code),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(sub("^(\\d{4}).*", "\\1", year)),
    value = na_if(value, ".."),
    value = as.numeric(gsub(",", "", value))
  )
View(data_demo_long)
data_demo_long <- data_demo_long %>%
  filter(!is.na(variable), variable != "")
data_demo_long <- data_demo_long %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(
    grepl("Age dependency ratio, old \\(\\%\\ of working-age population)", variable) ~ "Age_dep_old",
    grepl("Age dependency ratio, young \\(\\%\\ of working-age population)", variable) ~ "Age_dep_young",
    TRUE ~ variable
  ))
data_demo_wide <- data_demo_long %>%
  select(-variable_code) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = mean,       # or first()
    values_fill = NA
  )
View(data_demo_wide)

data_nfa <- read.csv("data/raw/NFA_2704.csv", header = TRUE, check.names = FALSE)
View(data_nfa)
data_nfa_long <- data_nfa %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    variable = `Series Name`,
    variable_code = `Series Code`
  ) %>%
  pivot_longer(
    cols = -c(country, country_code, variable, variable_code),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(sub("^(\\d{4}).*", "\\1", year)),
    value = na_if(value, ".."),
    value = as.numeric(gsub(",", "", value))
  )
View(data_nfa_long)
data_nfa_long <- data_nfa_long %>%
  filter(!is.na(variable), variable != "")
data_nfa_long <- data_nfa_long %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(
    grepl("Net foreign assets \\(current LCU)", variable) ~ "NFA",
    TRUE ~ variable
  ))
data_nfa_wide <- data_nfa_long %>%
  select(-variable_code) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = mean,       # or first()
    values_fill = NA
  )
View(data_nfa_wide)

data_instq <- read.csv("data/raw/Institutional_quality_2704.csv", header = TRUE, check.names = FALSE)
View(data_instq)
data_instq_long <- data_instq %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    variable = `Series Name`,
    variable_code = `Series Code`
  ) %>%
  pivot_longer(
    cols = -c(country, country_code, variable, variable_code),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(sub("^(\\d{4}).*", "\\1", year)),
    value = na_if(value, ".."),
    value = as.numeric(gsub(",", "", value))
  )
View(data_instq_long)
data_instq_long <- data_instq_long %>%
  filter(!is.na(variable), variable != "")
data_instq_long <- data_instq_long %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(
    grepl("Control of Corruption - Governance score\\(0-100)", variable) ~ "Control_corruption",
    grepl("Government Effectiveness - Governance score\\(0-100)", variable) ~ "Gov_effectiveness",
    grepl("Political Stability - Governance score\\(0-100)", variable) ~ "Pol_stability",
    grepl("Regulatory quality - Governance score\\(0-100)", variable) ~ "Reg_quality",
    grepl("Rule of law - Governance score\\(0-100)", variable) ~ "Rule_law",
    grepl("Voice and Accountability - Governance score\\(0-100)", variable) ~ "Voice_accountability",
    TRUE ~ variable
  ))
data_instq_wide <- data_instq_long %>%
  select(-variable_code) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = mean,       # or first()
    values_fill = NA
  )
View(data_instq_wide)

##Merge the data
list(wb_wide, data_er_wide, data_demo_wide, data_nfa_wide, data_instq_wide) %>%
  lapply(is.null)
combined <- reduce(list(wb_wide, data_er_wide, data_demo_wide, data_nfa_wide, data_instq_wide),
                  full_join,
                  by = c("country", "country_code", "year"))
View(combined)

combined <- combined %>%
  select(-`Data from database: World Development Indicators.x`, -`Data from database: World Development Indicators.y`, -`Data from database: World Development Indicators.x.x`, -`Data from database: World Development Indicators.y.y`, -`Last Updated: 04/08/2026.x`, -`Last Updated: 04/08/2026.y`, -`Last Updated: 04/08/2026.x.x`, -`Last Updated: 04/08/2026.y.y`)
combined <- combined %>%
  select(-`Last Updated: 04/08/2026.x`, -`Last Updated: 04/08/2026.y`, -`Last Updated: 04/08/2026.x.x`, -`Last Updated: 04/08/2026.y.y`)


