# International Macroeconomics

# Tutorial 1  : Current Account Determinants (Cross-Sectional OLS)
# Author      : Juan Pablo Ugarte Checura
# Date        : April 2026

# This script walks through a complete cross-country regression workflow:
#   1. Data acquisition from the World Bank (WDI)
#   2. Data cleaning (remove aggregates, handle missings)
#   3. Collapse panel to cross-section (country averages)
#   4. Visualization (scatter plots)
#   5. OLS regression (nested specifications)
#   6. Interpretation
#   7. Saving output

# Before running, make sure you have installed the required packages
# (see the slides or run the install block below).


# 0. Setup ---------------------------------------------------------------------

# Install packages (run once, then comment out)
# install.packages(c(
  # "WDI",          # World Bank data
  # "countrycode",  # Country name/code conversions
  # "plm",          # Panel data models (Tutorial 2)
  # "ggplot2",      # Plotting
  # "dplyr",        # Data manipulation
  # "tidyr",        # Reshaping data
  # "stargazer"     # LaTeX tables
 #))

library(WDI)
library(countrycode)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stargazer)

# Create output directories
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


# 1. Finding indicators --------------------------------------------------------

# Search for indicators by keyword (returns a lot of results!)
WDIsearch("current account")
WDIsearch("gdp per capita")
WDIsearch("net lending")
WDIsearch("trade")
WDIsearch("population growth")

# The codes we will use:
#   BN.CAB.XOKA.GD.ZS   -- Current account (% of GDP)
#   NY.GDP.PCAP.PP.KD   -- GDP per capita, PPP (constant 2021 int. $)
#   GC.NLD.TOTL.GD.ZS   -- Net lending (+) / net borrowing (-) (% of GDP)
#   NE.TRD.GNFS.ZS      -- Trade (% of GDP)
#   SP.POP.GROW         -- Population growth (annual %)


# 2. Data acquisition from the World Bank --------------------------------------

# We download the data
raw <- WDI(
  indicator = c(
    ca       = "BN.CAB.XOKA.GD.ZS",   # Current account (% of GDP)
    gdppc    = "NY.GDP.PCAP.PP.KD",   # GDP per capita, PPP (constant $)
    fiscal   = "GC.NLD.TOTL.GD.ZS",   # Fiscal balance (% of GDP)
    trade    = "NE.TRD.GNFS.ZS",      # Trade openness (% of GDP)
    pop_grow = "SP.POP.GROW"          # Population growth (%)
  ),
  country = "all",
  start = 2000, end = 2019
)

# Inspect the data
head(raw)                       # Look at the first few rows
str(raw)                        # Check variable types
nrow(raw)                       # Number of rows
length(unique(raw$country))     # Number of unique countries


# 3. Data cleaning -------------------------------------------------------------

# Remove aggregates (World, EU, Asia, etc.) using countrycode
raw <- raw %>%
  mutate(continent = countrycode(iso2c, "iso2c", "continent")) %>%
  filter(!is.na(continent))

# Check how many countries remain
length(unique(raw$country))

# Look at coverage of each variable (proportion of missing values)
raw %>%
  summarise(across(c(ca, gdppc, fiscal, trade, pop_grow),
                   ~mean(is.na(.x))))


# 4. Collapse to cross-section -------------------------------------------------

# Compute country-level averages over 2000-2019 (one observation per country)
df <- raw %>%
  group_by(country, iso2c) %>%
  summarise(
    n_years  = sum(!is.na(ca)),               # Count before averaging
    ca       = mean(ca, na.rm = TRUE),
    gdppc    = mean(gdppc, na.rm = TRUE),
    fiscal   = mean(fiscal, na.rm = TRUE),
    trade    = mean(trade, na.rm = TRUE),
    pop_grow = mean(pop_grow, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_years >= 10) %>%                   # Keep countries with enough data
  mutate(log_gdppc = log(gdppc)) %>%          # Log GDP per capita
  filter(abs(ca - mean(ca, na.rm = TRUE))
         < 3 * sd(ca, na.rm = TRUE))          # Remove outliers

# Inspect
head(df)      # Look at the first few rows
nrow(df)      # Number of countries in the final sample
summary(df)   # Summary statistics


# 5. Visualization: scatter plots ----------------------------------------------

# Scatter plot: Current account vs. log GDP per capita
p1 <- ggplot(df, aes(x = log_gdppc, y = ca)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#A0522D", se = TRUE) +
  geom_text(
    data = df %>% filter(iso2c %in% c("US", "CN", "DE", "SA", "BR", "JP")),
    aes(label = iso2c), nudge_y = 1.5, size = 3
  ) +
  labs(title = "Current Account vs. Income Level",
       x = "Log GDP per capita (PPP)",
       y = "Current Account (% of GDP)") +
  theme_minimal()

print(p1)

# Scatter plot: Current account vs. fiscal balance
p2 <- ggplot(df, aes(x = fiscal, y = ca)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#A0522D", se = TRUE) +
  geom_text(
    data = df %>% filter(iso2c %in% c("US", "CN", "DE", "SA", "BR", "JP")),
    aes(label = iso2c), nudge_y = 1.5, size = 3
  ) +
  labs(title = "Current Account vs. Fiscal Balance",
       x = "Fiscal Balance (% of GDP)",
       y = "Current Account (% of GDP)") +
  theme_minimal()

print(p2)


# 6. OLS regression ------------------------------------------------------------

# Model 1: Income only
model1 <- lm(ca ~ log_gdppc, data = df)

# Model 2: Add fiscal balance
model2 <- lm(ca ~ log_gdppc + fiscal, data = df)

# Model 3: Full specification
model3 <- lm(ca ~ log_gdppc + fiscal + trade + pop_grow, data = df)

# Print summaries
summary(model1)     # Income is positive and significant
summary(model2)     # Fiscal balance is positive and significant
summary(model3)     # Population growth is positive and significant

# Produce a clean comparison table
stargazer(model1, model2, model3,
          type = "text",
          title = "Current Account Determinants",
          dep.var.labels = "Current Account (\\% of GDP)",
          covariate.labels = c("Log GDP p.c.", "Fiscal balance",
                               "Trade openness", "Pop. growth"),
          digits = 3)


# 7. Interpretation ------------------------------------------------------------

# Questions to consider:
#
# - Which variables are statistically significant?
# - Do the signs match the theoretical priors?
# - How does R-squared change across models?
# - How many countries are in the sample?
#
# Limitations:
# - Endogeneity (GDP per capita and CA are jointly determined)
# - Omitted variables (net foreign assets, institutions, commodities...)
# - This is descriptive, not causal
# - We averaged over 20 years, throwing away all time variation


# 8. Saving output ------------------------------------------------------------

# Save figures
ggsave("output/figures/ca_vs_gdppc.pdf", p1, width = 8, height = 5)
ggsave("output/figures/ca_vs_fiscal.pdf", p2, width = 8, height = 5)

# Save regression table as LaTeX
stargazer(model1, model2, model3,
          type = "latex",
          out = "output/tables/ca_determinants.tex",
          title = "Current Account Determinants",
          dep.var.labels = "Current Account (\\% of GDP)",
          covariate.labels = c("Log GDP p.c.", "Fiscal balance",
                               "Trade openness", "Pop. growth"),
          digits = 3)

# Save processed data for reuse
write.csv(df, "data/processed/ca_cross_section.csv", row.names = FALSE)


# EXERCISES: Try it yourself!
#
# 1. Add another variable to the regression using WDI search.
#
# 2. Restrict the sample to a specific region (e.g., Europe, Latin America)
#    Do the results change?
#
# 3. Instead of averaging over 2000-2019, try two subperiods
#    Are the determinants stable over time?
#
# 4. Try a different dependent variable (e.g. trade balance)
#    Do you get similar results?

# end