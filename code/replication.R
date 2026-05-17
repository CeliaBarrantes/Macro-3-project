library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(tidyverse)
library(countrycode)
library(lubridate)
library(zoo)
library(ggplot2)

#Load data
dataset <- read.csv("data/processed/combined_4.csv",
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
  mutate(country = country.x) %>%
  select(-country.x, -country.y) #two country columns creating from the merging
dataset <- dataset %>%
  filter(nchar(code) == 3) #removing aggregate regions
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
    weighted_mean_growth = weighted.mean(
      growth_pc[!is.na(growth_pc) & !is.na(GDP_current)],
      GDP_current[!is.na(growth_pc) & !is.na(GDP_current)]
    ),

    world_fiscal_mean = weighted.mean(
      fiscal_balance[!is.na(fiscal_balance) & !is.na(GDP_current)],
      GDP_current[!is.na(fiscal_balance) & !is.na(GDP_current)]
    )
) %>% #we use a safer version of weighted.mean()
#to avoid issue in the treatment of NAs
  ungroup() %>%
  mutate(
    growth_dev = growth_pc - weighted_mean_growth,
    fiscal_dev = fiscal_balance - world_fiscal_mean,
    ca_gdp = CA / GDP_current
    )%>%
group_by(code)%>%
arrange(code, year)%>%
mutate(
    nfa_gdp_lagged = lag(nfa_gdp),
    openness_raw = (Exports + Imports) / GDP_current,
    fc_openness_raw = FCI * openness_raw,
    fc_openness_raw = replace_na(fc_openness_raw, 0)
)%>%
ungroup()%>%
group_by(year)%>%
mutate(

    world_youth_mean = weighted.mean(
      Age_dep_young[!is.na(Age_dep_young) & !is.na(GDP_current)],
      GDP_current[!is.na(Age_dep_young) & !is.na(GDP_current)]
    ),

    world_old_mean = weighted.mean(
      Age_dep_old[!is.na(Age_dep_old) & !is.na(GDP_current)],
      GDP_current[!is.na(Age_dep_old) & !is.na(GDP_current)]
    )
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
#We keep a version of the dataset with all the years
dataset_full <- dataset 
View(dataset_full)
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
#we should now check for the other variables
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
  )#we get 0 complete observations (without the later modifications)
sapply(data_rep, function(x) sum(is.na(x)))
#we first get three broken variables: age dep for both young and old, fiscal balance
#after modification, we are left with only the fiscal balance a bit broken but much less than before)

#Replication of Figure 1: in Gruber and Kamin (2007)
#1. Define the country lists
asia_countries <- c("China", "Hong Kong", "Indonesia", "South Korea", 
                    "Malaysia", "Philippines", "Singapore", "Taiwan", "Thailand")

advanced_economies <- c("United States", "Japan", "Germany", "France", "Italy", 
                        "United Kingdom", "Canada", "Australia", "Austria", 
                        "Belgium", "Denmark", "Finland", "Greece", "Iceland", 
                        "Ireland", "Netherlands", "New Zealand", "Norway", 
                        "Portugal", "Spain", "Sweden", "Switzerland")

# 2. Create the data for each line separately (to allow overlap)
# We use the dataset_full that doesn't have the time period intervals to replicate the figure.
us_data <- dataset_full %>%
  filter(country_standard == "United States") %>%
  group_by(year) %>%
  summarise(total_ca = sum(CA, na.rm = TRUE)/1e9) %>% # Divide by 1000,000,000 because CA is very big.
  mutate(group = "United States")

asia_data <- dataset_full %>%
  filter(country_standard %in% asia_countries) %>%
  group_by(year) %>%
  summarise(total_ca = sum(CA, na.rm = TRUE)/1e9) %>%
  mutate(group = "Developing Asia")

all_dev_data <- dataset_full %>%
  filter(!(country_standard %in% advanced_economies)) %>%
  group_by(year) %>%
  summarise(total_ca = sum(CA, na.rm = TRUE)/1e9) %>%
  mutate(group = "All Developing Countries")

# Combine them
plot_data <- bind_rows(us_data, asia_data, all_dev_data) %>%
  filter(year >= 1980 & year <= 2004)

# 4. Generate and save the Plot
ggsave("output/figures/figure1_rep.png", width = 10, height = 5, dpi = 300)
dev.new(width = 10, height = 5)
ggplot(plot_data, aes(x = year, y = total_ca, linetype = group)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c(
    "United States" = "solid", 
    "Developing Asia" = "dotted", 
    "All Developing Countries" = "longdash"
  )) +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(breaks = seq(1980, 2004, 2), limits = c(1980, 2004)) +
  scale_y_continuous(limits = c(-800, 450), breaks = seq(-800, 400, 200)) +
  labs(
    title = "Figure 1: Current Accounts",
    y = "Billions USD",
    x = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5 
  )

#Replication of Table 1
library(plm)
library(lmtest)
library(sandwich)

#We need panel data formatting
data_panel <-pdata.frame(data_rep, index = c("code", "period_id"))

#Model 1
m1 <- plm(CA_to_GDP ~ rel_income + growth_dev_avg + fiscal_balance_dev + nfa_gdp_initial + youth_dep_dev + old_dep_dev + openness + oil_balance,
          data = data_panel, model = "within", #fixed effects
          effect = "time") #time fixed effects only

#Model 2
data_rep <- data_rep %>%
  mutate(
    d_chine = ifelse(code == "CHN" & period_id == 4, 1, 0),
    d_hk = ifelse (code == "HKG" & period_id == 4, 1, 0),
    d_indonesia = ifelse (code == "IDN" & period_id == 4, 1, 0),
    d_korea = ifelse (code == "KOR" & period_id == 4, 1, 0),
    d_malaysia = ifelse (code == "MYS" & period_id == 4, 1, 0),
    d_phil = ifelse (code == "PHL" & period_id == 4, 1, 0),
    d_taiwan = ifelse (code == "TWN" & period_id == 4, 1, 0),
    d_thailand = ifelse (code == "THA" & period_id == 4, 1, 0),
    d_usa = ifelse (code == "USA" & period_id == 4, 1, 0),
  )
data_panel <-pdata.frame(data_rep, index = c("code", "period_id"))
m2 <- plm(CA_to_GDP ~ rel_income + growth_dev_avg + fiscal_balance_dev + nfa_gdp_initial + youth_dep_dev + old_dep_dev + openness + oil_balance + d_chine + d_hk + d_indonesia + d_korea + d_malaysia + d_phil + d_taiwan + d_thailand + d_usa,
          data = data_panel, model = "within", effect = "time")
#Model 3
m3 <- plm(CA_to_GDP ~ rel_income + growth_dev_avg + fiscal_balance_dev + nfa_gdp_initial + youth_dep_dev + old_dep_dev + openness + oil_balance + fc_dummy + fc_openness,
          data = data_panel, model = "within", effect = "time")
#Model 4
m4 <- plm(CA_to_GDP ~ rel_income + growth_dev_avg + fiscal_balance_dev + nfa_gdp_initial + youth_dep_dev + old_dep_dev + openness + oil_balance + fc_dummy + fc_openness + d_chine + d_hk + d_indonesia + d_korea + d_malaysia + d_phil + d_taiwan + d_thailand + d_usa,
          data = data_panel, model = "within", effect = "time")
#Model 5
m5 <- plm(CA_to_GDP ~ rel_income + growth_dev_avg + fiscal_balance_dev + nfa_gdp_initial + youth_dep_dev + old_dep_dev + openness + oil_balance + fc_dummy + fc_openness + d_usa,
          data = data_panel, model = "within", effect = "time")

#Build Table
extract_results <- function(model){
  coefs <- coef(summary(model))
  data.frame(
    variable = rownames(coefs),
    coef = coefs[, "Estimate"],
    tstat = coefs[, "t-value"]
  )
}
#Standard errors of the regressions
ser <- function(model){
  sqrt(deviance(model)/model$df.residual)
}
summary(m1)
#Define the row names for the table
library(modelsummary)
library(kableExtra)
coef_map <- c(
  "rel_income"          = "Per Capita GDP",
  "growth_dev_avg"      = "ΔGrowth",
  "fiscal_balance_dev"  = "Fiscal Balance",
  "nfa_gdp_initial"     = "NFA",
  "youth_dep_dev"       = "Youth Ratio",
  "old_dep_dev"         = "Elderly Ratio",
  "openness"            = "Openness",
  "oil_balance"         = "Oil Balance",
  "fc_dummy"            = "Fin. Crisis",
  "fc_openness"         = "Fin. Crisis*Openness",
  "d_china"             = "China(1997-2003)",
  "d_hk"                = "Hong Kong(1997-2003)",
  "d_indonesia"         = "Indonesia(1997-2003)",
  "d_korea"             = "Korea(1997-2003)",
  "d_malaysia"          = "Malaysia(1997-2003)",
  "d_phil"              = "Phil(1997-2003)",
  "d_taiwan"            = "Taiwan(1997-2003)",
  "d_thailand"          = "Thailand(1997-2003)",
  "d_us"                = "U.S.(1997-2003)"
)
#Formatting table
models <- list("1" = m1, "2" = m2, "3" = m3, "4" = m4, "5" = m5)
options(modelsummary_factory_html = "kableExtra")

install.packages("xtable")
library(xtable)

# Extract coefficients manually
coefs <- lapply(models, function(m) coef(summary(m)))
print(coefs)  # check it works first

# Then save as HTML
sink("output/tables/table1.html")
for(i in seq_along(models)){
  cat(paste0("<h3>Model ", i, "</h3>"))
  print(xtable(coefs[[i]]), type = "html")
}
sink()