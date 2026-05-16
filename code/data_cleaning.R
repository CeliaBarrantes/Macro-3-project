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

#reconstructing the oil balances
#we download the oil quantities data from Our World in Data
production_url <- "https://ourworldindata.org/grapher/oil-production-by-country.csv"
consumption_url <- "https://ourworldindata.org/grapher/oil-consumption-by-country.csv"
library(tidyverse)
library(countrycode)
#we load the data
prod <- read_csv("https://ourworldindata.org/grapher/oil-production-by-country.csv")
cons <- read_csv("https://ourworldindata.org/grapher/oil-consumption-by-country.csv")
View(prod)
View(cons)
names(prod)
names(cons)
prod <- prod %>%
  rename(production = Oil)
cons <- cons %>%
  rename(consumption = Oil)
#we merge production and consumption
oil <- prod %>%
 select(Entity, Code, Year, production) %>%
 full_join(
  cons %>% select(Entity, Code, Year, consumption),
  by = c("Entity", "Code", "Year")
 )
#we approximate net exports by computing production - consumption
oil <- oil %>%
 mutate(net_oil = production - consumption)
View(oil)
#we convert to value
price <- read_csv("https://raw.githubusercontent.com/datasets/oil-prices/master/data/brent-daily.csv")
library(dplyr)
library(lubridate)
price <- price %>%
 mutate(Year = year(as.Date(Date))) %>%
 group_by(Year) %>%
 summarise(oil_price_usd_per_barrel = mean(Price, na.rm = TRUE))
oil <- oil %>%
 left_join(price, by = "Year") %>%
 mutate(
  net_oil_barrels = net_oil * 0.172 * 1e6,
  oil_balance_value = net_oil_barrels * oil_price_usd_per_barrel
 )
names(combined)
names(oil)
oil <- oil %>%
 rename(code = Code, year = Year)
combined <- combined %>%
 rename(code = country_code, year = year)

combined <- combined %>%
 left_join(
  oil %>% select(code, year, oil_balance_value),
  by = c("code", "year")
 )
combined %>% 
 select(oil_balance_value.x, oil_balance_value.y) %>%
 summary() 
#they are identical
combined <- combined %>% 
mutate(oil_balance_value = oil_balance_value.y) %>%
select(-oil_balance_value.x, -oil_balance_value.y) 
combined <- combined %>% 
  mutate(oil_balance_gdp = oil_balance_value / GDP_current )
summary(combined$oil_balance_gdp)
view(combined)
write.csv(combined, "data/processed/combined_2.csv", row.names = FALSE)

#adding the financial crisis dummy to the data by countrycode
install.packages("janitor")
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(janitor)
combined <- read_csv("data/processed/combined_3.csv")
fb <- read_excel("data/raw/fb.xlsx")
View(fb)
names(fb)
#we need to reshape the fb data to get it the right format (from wide to long)
fb <- fb %>%
  rename(code = country_code)
fb_long <- fb %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "dummy"
  )
fb_long <- fb_long %>%
  mutate(
    year = as.numeric(gsub("x", "", year))
  ) #we clean the year format (remove the x in front)
View(fb_long)
View(combined)
combined <- combined %>%
  select(-any_of(c("dummy", "Dummy.x", "Dummy.y")))
#we removed old crisis variables
View(combined)
combined_4 <- combined %>%
  left_join(
    fb_long,
    by = c("code", "year")
  ) #we merge by country code
View(combined_4)
summary(combined_4$dummy)
sum(is.na(combined_4$dummy))
length(
  intersect(
    unique(combined$code),
    unique(fb_long$code)
  )
) #we get around 54 matching code names
write_csv(combined_4, "data/processed/combined_4.csv")

library(dplyr)
library(readxl)

#Constructing the FCI 
combined_4 <- read.csv("data/processed/combined_4.csv")

combined_4 <- combined_4 %>%
select(-any_of(c("FCI", "dummy")))

major <- read_excel("data/raw/SystemicMajor.xlsx")
border <- read_excel("data/raw/SystemicBorderline.xlsx")

major <- major %>%
 rename(
  code = `Country Code`,
  year = Year,
  dummy = Dummy
 ) %>%
 select(code, year, dummy)

border <- border %>%
 rename(
  code = `Country Code`,
  year = Year,
  dummy = Dummy
 ) %>%
 select(code, year, dummy)

crisis <- bind_rows(major, border)

crisis <- crisis %>%
 filter(code != "SGP")

sample <- unique(crisis$code)

fci_data <- combined_4 %>%
  filter(code %in% sample_codes) %>%
  left_join(crisis, by = c("code", "year"))

fci_data <- fci_data %>%
  mutate(dummy = ifelse(is.na(dummy), 0, dummy))

fci_data <- fci_data %>%
  group_by(year) %>%
  mutate(
    sample_GDP = sum(GDP_current, na.rm = TRUE),
    weighted_GDP = sum(GDP_current * dummy, na.rm = TRUE),
    weighted_GDP_ratio = weighted_GDP / sample_GDP,
    FCI = dummy - weighted_GDP_ratio
  ) %>%
  ungroup()

fci_final <- fci_data %>%
  select(code, year, FCI) %>%
  group_by(code, year) %>%
  summarise(FCI = first(FCI), .groups = "drop")

combined_4 <- combined_4 %>%
  left_join(fci_final, by = c("code", "year"))


##Constructing the fiscal balance ratio 

sample_codes <- unique(crisis$code)

fiscal_sample <- combined_4 %>%
  filter(code %in% sample_codes) %>%
  mutate(fiscal_balance_gdp = fiscal_balance / GDP_current) %>%
  group_by(year) %>%
  mutate(
    sample_fiscal_mean = sum(fiscal_balance_gdp * GDP_current, na.rm = TRUE) /
      sum(GDP_current[!is.na(fiscal_balance_gdp)], na.rm = TRUE),
    fiscal_balance_dev = fiscal_balance_gdp - sample_fiscal_mean
  ) %>%
  ungroup() %>%
  select(code, year, fiscal_balance_gdp, fiscal_balance_dev)

combined_4 <- combined_4 %>%
  left_join(fiscal_sample, by = c("code", "year"))

write.csv(combined_4, "data/processed/combined_4.csv", row.names = FALSE)

##Converting the NFA from LCU to USD
sample_codes <- unique(crisis$code)
nfa_sample <- combined_4 %>%
  filter(code %in% sample_codes) %>%
  select(code, year, NFA, Exchange_rate, GDP_current) %>%
  distinct(code, year, .keep_all = TRUE) %>%
  mutate(
    NFA_usd = NFA / Exchange_rate,
    nfa_gdp = NFA_usd / GDP_current
  ) %>%
  select(code, year, NFA_usd, nfa_gdp)

combined_4 <- combined_4 %>%
  left_join(nfa_sample, by = c("code", "year"))

write.csv(combined_4, "data/processed/combined_4.csv", row.names = FALSE)