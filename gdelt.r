#
# CA683 - Assignment 2 - Main script
#

# Packages Used
# install.packages("GDELTtools")
# install.packages("xts")
require("GDELTtools")
require("xts")

# ##########################################################################
# 1. Download GDELT data
#    - Use base year 2000 just for initial analysis
#    - Download all your gdelt zip files from:
#      http://data.gdeltproject.org/events/index.html
#    - Place the downloaded files in the "./data/gdelt/" folder then the
#      GetGDELT command will get the files directly from there and will not
#      try to re-download them.
#
# Warning: The file 2014-03-19 is MISSING.
# So when downloading never use a date interval containing that date.

# Filter:
# - Root Events Only: consider only headline of news article
# - ActionGeo Countries: Iran, Iraq, Kuwait, Oman, Qatar, Saudi Arabia, Syria and United Arab Emirates
# - Get all event codes for now. We will filter at a later stage
# gdelt_filter <- list(
#   IsRootEvent=1,
#   ActionGeo_CountryCode=c("IR", "IQ", "KW", "OM", "QA", "SA", "SY", "AE"))

# Download (or load zip if existent)
# gdelt <- GetGDELT(
#   start.date="2000-01-01",
#   end.date="2000-12-31",
#   filter=gdelt_filter,
#   local.folder="./data/gdelt/")

# Save pre-filtered GDELT database
# saveRDS(gdelt, "gdelt_2000.rds")

# Load pre-filtered GDELT database
gdelt <- readRDS("./data/gdelt_2000.rds")

# Add Date column for time series analysis
gdelt$Date <- as.Date(as.character(gdelt$SQLDATE), format="%Y%m%d")

# ##########################################################################
# 2. Filter GDELT events
# Remove duplicates
# gdelt_nodups <- gdelt[!duplicated(gdelt[,c("DATEADDED", "ActionGeo_CountryCode", "EventCode", "SOURCEURL")]),]

# Get only events with significant GoldsteinScale [-10.0,10.0]
# gdelt_significant <- gdelt[abs(gdelt$GoldsteinScale) >= 8.0,]

# Get only unconvencial violence events (mass killings, etc.)
# And remove unused columns
gdelt_filtered <- gdelt[
  gdelt$EventRootCode==20,
  c("Date", "EventCode", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "ActionGeo_CountryCode", "SOURCEURL")]

# Aggregate events by date and count
gdelt_summary.count <- aggregate(gdelt_filtered, by = list(gdelt_filtered$Date), length)
gdelt_summary.mean <- aggregate(gdelt_filtered, by = list(gdelt_filtered$Date), mean, na.action = na.omit)

# Merge all GDELT indicators into one variable
gdelt_indicators <- cbind(
  # Event Count
  xts(gdelt_summary.count[,c("Date")],gdelt_summary.count[,1]),
  # Event Means
  xts(gdelt_summary.mean[,c("GoldsteinScale", "NumMentions", "NumArticles", "AvgTone")], gdelt_summary.mean[,1])
)
# Normalize Names
names(gdelt_indicators) <- c("EventCount", "GoldsteinScale", "NumMentions", "NumArticles", "AvgTone")

# Save GDELT working object
saveRDS(gdelt_indicators, "./data/gdelt_indicators.rds")

# Plot the GDELT indicators
plot.zoo(gdelt_indicators, col=1:5,main="GDELT Indicators")

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
