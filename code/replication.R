library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(tidyverse)
library(countrycode)
library(lubridate)
library(zoo)

#Load data
dataset <- read.csv("data/processed/combined_3.csv",
                       header = TRUE,
                       check.names = FALSE)
View(dataset)
#Define the time periods
dataset <- dataset%>%
  mutate(period_replication = case_when(
    year >= 1982 & year <= 1986 ~ "1982_1986",
    year >= 1987 & year <= 1991 ~ "1987_1991",
    year >= 1992 & year <= 1996 ~ "1992_1996",
    year >= 1997 & year <= 2003 ~ "1997_2003",
    TRUE ~ NA_character_
  ))

#We need first to restrict our data to the paper sample
#We include only the countries in Gruber and Kamin (2007)
paper_sample <- c(
  "Argentina", "Australia", "Austria", "Bahrain", "Bangladesh", "Belgium", 
  "Bolivia", "Brazil", "Canada", "Chile", "China", "Colombia", "Costa Rica", 
  "Cyprus", "Denmark", "Dominican Republic", "Egypt", "El Salvador", 
  "Finland", "France", "Germany", "Greece", "Guatemala", "Haiti", 
  "Honduras", "Hong Kong", "Iceland", "India", "Indonesia", "Ireland", 
  "Israel", "Italy", "Japan", "Jordan", "South Korea", "Malaysia", 
  "Mexico", "Netherlands", "New Zealand", "Norway", "Oman", "Pakistan", 
  "Panama", "Paraguay", "Peru", "Philippines", "Portugal", "Singapore", 
  "South Africa", "Spain", "Sri Lanka", "Sweden", "Switzerland", "Syria", 
  "Taiwan", "Thailand", "Turkey", "United Kingdom", "United States", "Venezuela"
)
#Define the custom match to handle that specific long string
custom_names <- c("Hong Kong Special Administrative Region, People's Republic of China" = "Hong Kong")

dataset <- dataset %>%
  mutate(country_standard = countrycode(
    country, 
    origin = "country.name", 
    destination = "country.name",
    custom_match = custom_names  # This tells the function: "If you see the long name, use 'Hong Kong'"
  )) %>%
  mutate(country_standard = case_when(
    grepl("Korea", country, ignore.case = TRUE) ~ "South Korea",
    grepl("Hong", country, ignore.case = TRUE)  ~ "Hong Kong",
    TRUE ~ country_standard
  ))
dataset <- dataset %>%
  filter(country_standard %in% paper_sample)
missing_countries <- setdiff(paper_sample, unique(dataset$country_standard))
print(missing_countries)

### DEFINE VARIABLES OF THE PAPER
#Relative income wrt to average global GDP
dataset <- dataset %>%
    group_by(year)%>%
    mutate(mean_gdppc = mean(GDP_pc_const, na.rm = TRUE),
            rel_income_raw = GDP_pc_const / mean_gdppc) %>%
    ungroup()%>%
  arrange(code, year) %>%
  group_by(code) %>%
  mutate(growth_pc = log(GDP_pc_const) - log(lag(GDP_pc_const))) %>%
  ungroup()%>%
  group_by(year) %>%
  mutate(
    weighted_mean_growth = weighted.mean(growth_pc, GDP_current, na.rm = TRUE),
    world_fiscal_mean = weighted.mean(fiscal_balance, GDP_current, na.rm = TRUE)
    ) %>%
  ungroup() %>%
  mutate(
    growth_dev = growth_pc - weighted_mean_growth,
    fiscal_dev = fiscal_balance - world_fiscal_mean,
    ca_gdp = CA / GDP_current
    )%>%
group_by(code)%>%
arrange(code, year)%>%
mutate(
    nfa_gdp = NFA/GDP_current,
    nfa_gdp_lagged = lag(nfa_gdp),
    openness_raw = (Exports + Imports) / GDP_current,
    fc_openness_raw = FCI * openness_raw,
    fc_openness_raw = replace_na(fc_openness_raw, 0)
)%>%
ungroup()%>%
group_by(year)%>%
mutate(
    world_youth_mean = weighted.mean(Age_dep_young, GDP_current, na.rm = TRUE),
    world_old_mean   = weighted.mean(Age_dep_old, GDP_current, na.rm = TRUE)
)%>%
ungroup()%>%
mutate(
    youth_dev = Age_dep_young - world_youth_mean,
    old_dev = Age_dep_old - world_old_mean
)%>%
ungroup()

#Adapt to time periods
data_rep <- dataset %>%
  filter(!is.na(period_replication)) %>%
  group_by(code, period_replication) %>%
  summarise(
    growth_dev_avg = mean(growth_dev, na.rm = TRUE), #Change in growth rate of real income
    CA_to_GDP = mean(ca_gdp, na.rm = TRUE), #CA to GDP ratio
    rel_income = mean(rel_income_raw, na.rm = TRUE),#Real income
    fiscal_balance_dev = mean(fiscal_dev, na.rm = TRUE), #Fiscal balance deviation
    nfa_gdp_initial = first(na.omit(nfa_gdp_lagged)), #we take the first lagged value found in the period
    youth_dep_dev = mean(youth_dev, na.rm = TRUE),
    old_dep_dev = mean(old_dev, na.rm = TRUE),
    openness = mean(openness_raw, na.rm = TRUE),
    oil_balance = mean(oil_balance_gdp, na.rm = TRUE),
    fc_dummy = mean(FCI, na.rm = TRUE),
    fc_openness = mean(fc_openness_raw, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(period_id = case_when(
    period_replication == "1982_1986" ~ 1,
    period_replication == "1987_1991" ~ 2,
    period_replication == "1992_1996" ~ 3,
    period_replication == "1997_2003" ~ 4
  ))

data_rep <- data_rep %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
#Drop the years outside replication period
dataset <- dataset %>%
  filter(year >= 1982 & year <= 2003)
View(data_rep)

#Check quality of the data
#Check coverage by country and period
data_quality_table <- data_rep %>%
  group_by(period_replication) %>%
  summarise(
    total_countries = n(),
    missing_CA = sum(is.na(CA_to_GDP)),
    missing_Growth_Change = sum(is.na(growth_dev_avg)),
    missing_NFA = sum(is.na(nfa_gdp_initial)),
    missing_Oil = sum(is.na(oil_balance))
  )
View(data_quality_table)
print(data_quality_table)
#We should now check for other missing variables 
summary(data_rep$growth_dev_avg)
sum(is.na(data_rep$growth_dev_avg))
#we loose around 86obs from this
data_rep %>%
  summarise(
    total_obs = n(),
    complete_obs = sum(
      complete.cases(
        CA_to_GDP,
        rel_income,
        growth_dev_avg,
        fiscal_balance_dev,
        nfa_gdp_initial,
        youth_dep_dev,
        old_dep_dev,
        openness,
        oil_balance,
        fc_dummy,
        fc_openness
      )
    )
  )
#we get 0 complete observations
sapply(data_rep, function(x) sum(is.na(x)))
#we get three broken variables