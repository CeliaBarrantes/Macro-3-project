library(dplyr)
weo <- read.csv("data/raw/WEO_April2026.csv")
names(weo)
vars <- c("Gross domestic product (GDP), Current prices, US dollar",
                            "Population, Persons for countries / Index for country groups",
                            "Employed persons, Persons for countries / Index for country groups",
                            "Unemployment rate",
                            "Change in reserve assets, Net (assets minus liabilities), US dollar",
                            "Current account balance (credit less debit), US dollar",
                            "Gross domestic product (GDP), Per capita, purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021",
                            "Output gap, Percent of potential GDP",
                            "Imports of goods and services, US dollar",
                            "Exports of goods and services, US dollar",
                            "Gross domestic product (GDP), Current prices, Purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021",
                            "WTI crude, Unit prices, US dollars per barrel",
                            "Gross domestic product (GDP), Current prices, Per capita, US dollar",
                            "Brent crude, Unit prices, US dollars per barrel",
                            "Dubai crude, Unit prices, US dollars per barrel")
weo_small <- weo %>%
  filter("INDICATOR" %in% vars)
write.csv(weo_small, "data/processed/weo_clean.csv", row.names = FALSE)