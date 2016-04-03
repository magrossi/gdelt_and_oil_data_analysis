#
# CA683 - Assignment 2 - Main script
#

# Packages Used
# install.packages("xts")
require("xts")
source("gdelt_loader.r")

# ##########################################################################
# 1. Download GDELT data
#    - Use base year 2000 just for initial analysis
#    - Download all your gdelt zip files from:
#      http://data.gdeltproject.org/events/index.html
#    - Place the downloaded files in the "./data/gdelt/" folder then the
#      GetGDELT command will get the files directly from there and will not
#      try to re-download them.
#
# Warning: The file 2014-03-19 is MISSING. [loader function takes care of this now]
#          So when downloading never use a date interval containing that date.

if (!(file.exists("./data/gdelt_indicators.rds"))) {
  # Filter:
  # - Root Events Only: consider only headline of news article
  # - ActionGeo Countries: Iran, Iraq, Kuwait, Oman, Qatar, Saudi Arabia, Syria and United Arab Emirates
  # - Get all event codes for now. We will filter at a later stage
  gdelt_filter <- list(
    IsRootEvent=1,
    ActionGeo_CountryCode=c("IR", "IQ", "KW", "OM", "QA", "SA", "SY", "AE"))
  
  # Download (or load zip if existent) yearly gdelt data, save to gdelt_{year}.rds and combine the result
  # in the gdelt data frame
  # Takes a very very very long time if you don't have the gdelt_{year}.rds files already
  gdelt <- load_gdelt_year_auto(2000, 2016, gdelt_filter)

  # Save GDELT working object
  saveRDS(gdelt, "./data/gdelt_indicators.rds")
} else {
  # Load GDELT working object
  gdelt <- readRDS("./data/gdelt_indicators.rds")
}
  
# Plot the GDELT indicators
plot.zoo(gdelt, col=1:5,main="GDELT Indicators")

# Uncomment this line if you want to de-normalize the count data
# due to the exponential increase in news global coverage over time
# gdelt_daily_country <- readRDS("./data/gdelt_daily_counts_all_events_all_countries.rds")

# ##########################################################################
# 3. Load Oil and Derivates data
diesel_ny_daily <- readRDS("./data/diesel_ny_daily.rds")
diesel_us_gulf_coast_daily <- readRDS("./data/diesel_us_gulf_coast_daily.rds")
diesel_us_la_daily <- readRDS("./data/diesel_us_la_daily.rds")
gasoline_ny_daily <- readRDS("./data/gasoline_ny_daily.rds")
gasoline_us_gulf_cost_daily <- readRDS("./data/gasoline_us_gulf_cost_daily.rds")
gasoline_us_la_daily <- readRDS("./data/gasoline_us_la_daily.rds")
heating_oil_ny_daily <- readRDS("./data/heating_oil_ny_daily.rds")
henry_hub_natural_gas_daily <- readRDS("./data/henry_hub_natural_gas_daily.rds")
kerosene_jet_fuel_daily <- readRDS("./data/kerosene_jet_fuel_daily.rds")
oil_brent_daily <- readRDS("./data/oil_brent_daily.rds")
oil_canada_monthly <- readRDS("./data/oil_canada_monthly.rds")
oil_fateh_monthly <- readRDS("./data/oil_fateh_monthly.rds")
oil_opec_daily <- readRDS("./data/oil_opec_daily.rds")
oil_wti_daily <- readRDS("./data/oil_wti_daily.rds")
propane_mont_belvieu_daily <- readRDS("./data/propane_mont_belvieu_daily.rds")

# Merge Oil & Derivates into one multivariate time series
# Using na.approx method to fill missing values
oil_and_derivates <- na.approx(cbind(diesel_ny_daily,
      diesel_us_gulf_coast_daily,
      diesel_us_la_daily,
      gasoline_ny_daily,
      gasoline_us_gulf_cost_daily,
      gasoline_us_la_daily,
      heating_oil_ny_daily,
      henry_hub_natural_gas_daily,
      kerosene_jet_fuel_daily,
      oil_brent_daily,
      oil_canada_monthly,
      oil_fateh_monthly,
      oil_opec_daily,
      oil_wti_daily,
      propane_mont_belvieu_daily))

# Plot Oil & Derivates side by side
plot.zoo(oil_and_derivates,
         col=1:length(names(oil_and_derivates)),
         main="Oil & Derivates")

