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

### DEFINE VARIABLES OF THE PAPER
#Relative income wrt to average global GDP
mean_gdppc <- mean(dataset$GDP_pc_const, na.rm = TRUE)
dataset <- dataset %>%
  mutate(rel_income_raw = GDP_pc_const / mean_gdppc)

#Growth rate of real income
dataset <- dataset %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(growth_pc = log(GDP_pc_const) - log(lag(GDP_pc_const))) %>%
  ungroup()

#GDP-weighted deviation of real income
dataset <- dataset %>%
  group_by(year) %>%
  mutate(weighted_mean_growth = weighted.mean(growth_pc, GDP_current, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(growth_dev = growth_pc - weighted_mean_growth)

#Adapt to time periods
data_rep <- dataset %>%
  filter(!is.na(period_replication)) %>%
  group_by(country, period_replication) %>%
  summarise(
    growth_dev_avg = mean(growth_dev, na.rm = TRUE), #Change in growth rate of real income
    CA_to_GDP = mean(CA / GDP_current, na.rm = TRUE), #CA to GDP ratio
    rel_income = mean(rel_income_raw, na.rm = TRUE), #Real income
    .groups = "drop"
  )

#Period index
data_rep <- data_rep %>%
  mutate(period_id = case_when(
    period_replication == "1982_1986" ~ 1,
    period_replication == "1987_1991" ~ 2,
    period_replication == "1992_1996" ~ 3,
    period_replication == "1997_2003" ~ 4
  ))

#Change in growth rate
data_rep <- data_rep %>%
  arrange(country, period_id) %>%
  group_by(country) %>%
  mutate(growth_change = growth_dev_avg - lag(growth_dev_avg)) %>%
  ungroup()

#Fiscal balance
dataset <- dataset %>%
  group_by(year) %>%
  mutate(world_fiscal_mean = weighted.mean(fiscal_balance,
                                            GDP_current,
                                            na.rm = TRUE)) %>%
  ungroup()
dataset <- dataset %>%
  mutate(fiscal_dev = fiscal_balance - world_fiscal_mean)
data_rep <- dataset %>%
  filter(!is.na(period_replication)) %>%
  group_by(country, period_replication) %>%
  summarise(
    fiscal_balance_dev = mean(fiscal_dev, na.rm = TRUE),
    .groups = "drop"
  )
View(data_rep)