library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(tidyverse)
library(countrycode)
library(lubridate)
library(zoo)

dataset <- read.csv("data/processed/combined_3.csv", header = TRUE, check.names = FALSE)
View(dataset)

#Building the variables
names(combined_3)
summary(combined_3$GDP_current)
summary(combined_3$year)
combined_3 <- combined_3 %>% #set the year periods
  mutate(period_extension = case_when(
    year >= 1960 & year <= 1964 ~ "1960_1964",
    year >= 1965 & year <= 1969 ~ "1965_1969",
    year >= 1970 & year <= 1974 ~ "1970_1974",
    year >= 1975 & year <= 1979 ~ "1975_1979",
    year >= 1980 & year <= 1984 ~ "1980_1984",
    year >= 1985 & year <= 1989 ~ "1985_1989",
    year >= 1990 & year <= 1994 ~ "1990_1994",
    year >= 1995 & year <= 1999 ~ "1995_1999",
    year >= 2000 & year <= 2004 ~ "2000_2004",
    year >= 2005 & year <= 2009 ~ "2005_2009",
    year >= 2010 & year <= 2014 ~ "2010_2014",
    year >= 2015 & year <= 2019 ~ "2015_2019",
    year >= 2020 & year <= 2025 ~ "2020_2025",
    TRUE ~ NA_character_
  ))
combined_3$CA_to_GDP <- combined_3$CA / combined_3$GDP_current #CA to GDP ratio
mean_gdppc <- mean(combined_3$GDP_pc_const, na.rm = TRUE) #Overall mean GDP
combined_3$rel_income <- combined_3$GDP_pc_const / mean_gdppc #Real per capita income to mean GDP

combined_3 <- combined_3 %>% #Growth rates of real income 
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(growth_pc = (log(GDP_pc_const) - log(lag(GDP_pc_const)))) %>%
  ungroup()
combined_3 <- combined_3 %>% #GDP-weighted mean growth
  group_by(year) %>%
  mutate(weighted_mean_growth = weighted.mean(growth_pc, GDP_current, na.rm = TRUE)) %>%
  ungroup()
combined_3 <- combined_3 %>% #Deviations from mean
  mutate(growth_dev = growth_pc - weighted_mean_growth)
combined_3 <- combined_3 %>% #multi-year average + change
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    growth_dev_ma = rollapply(growth_dev, 5, mean, align = "right", fill = NA, na.rm = TRUE),
    growth_change = growth_dev_ma - lag(growth_dev_ma)
  ) %>%
  ungroup() #we use growth_change in our regression