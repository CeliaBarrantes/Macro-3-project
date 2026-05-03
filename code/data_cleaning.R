library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(tidyverse)
library(countrycode)
library(lubridate)
library(zoo)

##Restructing the data
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


fiscal_b <- read.csv("data/raw/FiscalBalance_54countries.csv", header = TRUE, check.names = FALSE)
View(fiscal_b)
names(fiscal_b)
data_fb_long <- fiscal_b %>%
  rename(
    country = COUNTRY
  ) %>%
  
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "value"
  ) %>%
  
  # CRITICAL FIX: force character BEFORE cleaning
  mutate(
    year = as.numeric(year),
    value = as.character(value)
  )
data_fb_long <- data_fb_long %>%
  mutate(
    value = na_if(value, ".."),
    value = as.numeric(gsub(",", "", value))
  )%>%
  mutate(
    variable = "Fiscal balance (% of GDP)",
    variable_code = DATASET
  ) %>%
  mutate(
    country_code = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Hong Kong Special Administrative Region, People's Republic of China" = "HKG"
      )
    )
  ) %>%
  filter(!is.na(country), country != "")
data_fb_long <- data_fb_long %>%
  transmute(
    country,
    country_code,
    variable,
    variable_code,
    year,
    value
  ) %>%
  filter(!is.na(country), country != "")
data_fb_wide <- data_fb_long %>%
  select(-variable_code) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = mean,
    values_fill = NA
  )
View(data_fb_wide)
data_fb_wide <- data_fb_wide %>%
  rename(
    fiscal_balance = `Fiscal balance (% of GDP)`
  )

##Merge the data
combined <- reduce(
  list(
    wb_wide,
    data_er_wide,
    data_demo_wide,
    data_nfa_wide,
    data_instq_wide,
    data_fb_wide
  ),
  full_join,
  by = c("country", "country_code", "year")
)

View(combined)

combined <- combined %>%
  select(-`Data from database: World Development Indicators.x`, -`Data from database: World Development Indicators.y`, -`Data from database: World Development Indicators.x.x`, -`Data from database: World Development Indicators.y.y`, -`Last Updated: 04/08/2026.x`, -`Last Updated: 04/08/2026.y`, -`Last Updated: 04/08/2026.x.x`, -`Last Updated: 04/08/2026.y.y`)
combined <- combined %>%
  select(-`Last Updated: 04/08/2026.x`, -`Last Updated: 04/08/2026.y`, -`Last Updated: 04/08/2026.x.x`, -`Last Updated: 04/08/2026.y.y`)

#reconstructing the oil balances
#we download the oil quantities data from Our World in Data
production_url <- "https://ourworldindata.org/grapher/oil-production-by-country.csv"
consumption_url <- "https://ourworldindata.org/grapher/oil-consumption-by-country.csv"

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
  mutate(oil_balance_gdp = oil_balance_value / GDP_current ) #in the paper we use oil balance wrt gdp
summary(combined$oil_balance_gdp)
view(combined)

##Creating the financial index

#merging the dataset for the banking crisis
major <- read_excel("data/raw/SystemicMajor.xlsx")
border <- read_excel("data/raw/SystemicBorderline.xlsx")

#matching the column names
major <- major %>% rename(country = Country, year=Year)
border <- border %>% rename(country = Country, year=Year)
major <- major %>% mutate(year=as.numeric(year))
border <- border %>% mutate(year=as.numeric(year))
combined <- combined %>% mutate(year=as.numeric(year))

#merging again
crisis <- full_join(major, border, by = c("country", "year"))
combined_3 <- combined %>% left_join(crisis, by = c("country", "year"))
write.csv(combined_3, "data/processed/combined_3.csv", row.names = FALSE)

#merging the dummy variable as one instead of .x, .y
combined_3 <- combined_3 %>%
 mutate(
  dummy = as.numeric((Dummy.x == 1)|(Dummy.y == 1))
 ) %>%
 select(-Dummy.x, -Dummy.y)

#constructing the index
#step 1: computing total sample GDP and weighted GDP
sample <- combined_3 %>% 
 semi_join(crisis, by = c("country", "year")) %>%
 left_join(crisis, by = c("country", "year")) %>%
 group_by(year) %>%
 summarise(
  sample_GDP = sum(GDP_current, na.rm = TRUE), 
  crisis_GDP = sum(GDP_current * dummy, na.rm = TRUE),
  weighted_GDP = crisis_GDP/sample_GDP,
  .groups = "drop")

#step 2: the Financial Crisis Index (FCI)
combined_3 <- combined_3 %>%
 left_join(crisis, by = c("country", "year")) %>%
 left_join(sample %>% select(year, weighted_GDP), by = "year") %>%
 mutate(FCI = dummy - weighted_GDP)

#checks 
View(combined_3)
summary(combined_3$FCI)
summary(combined_3$dummy)
write.csv(combined_3, "data/processed/combined_3.csv", row.names = FALSE)
