library(sf)
library(tidyverse)
library(quantmod)

# Read the shapefile
power_plants <- st_read("PowerPlants_US_EIA/PowerPlants_US_202108.shp")

# Keep only mainland U.S.
mainland_power_plants <- power_plants %>%
  filter(!StateName %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  mutate(
    Fuel_Category = case_when(
      PrimSource %in% c(
        "natural gas",
        "coal",
        "nuclear",
        "hydroelectric",
        "wind",
        "solar",
        "petroleum"
      ) ~ PrimSource,
      TRUE ~ "other"
    ),
    Fuel_Category = str_to_title(Fuel_Category)
  )

tickers <- c("NEE", "DUK", "SO", "CEG")

getSymbols(
  Symbols = tickers,
  src = "yahoo",
  from = "2020-01-01",
  auto.assign = TRUE
)

stock_prices <- do.call(
  merge,
  lapply(tickers, function(x) Cl(get(x)))
)

colnames(stock_prices) <- tickers

save(mainland_power_plants, stock_prices, file = "power_plants.RData")
# Set factor order
mainland_power_plants$Fuel_Category <- factor(
  mainland_power_plants$Fuel_Category,
  levels = c(
    "Natural Gas",
    "Coal",
    "Nuclear",
    "Hydroelectric",
    "Wind",
    "Solar",
    "Petroleum",
    "Other"
  )
)

# Save cleaned dataset
save(mainland_power_plants, file = "power_plants.RData")

