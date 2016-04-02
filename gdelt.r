
#install.packages("GDELTtools")
library("GDELTtools")
library("tseries")
library("xts")

#test.filter <- list(Actor1KnownGroupCode="OPC", EventBaseCode="101, 08, 18, 19, 20, 08, 14, 15, 17")
#test.results <- GetGDELT(start.date="2000-01-01", end.date="2013-12-31", filter=test.filter, local.folder="../datasets/", allow.wildcards = FALSE, use.regex = TRUE, data.url.root = "http://data.gdeltproject.org/events/", verbose = TRUE)


## Not run:
# 1=Verbal Cooperation, 2=Material Cooperation, 3=Verbal Conflict, 4=Material Conflict.
#EventCode=c(), EventBaseCode=c(), EventRootCode=c()
#GoldsteinScale=-10.0, #c(seq(8.0,10.0,by=0.1),seq(-8.0,-10.0,by=-0.1)),
filter <- list(
  IsRootEvent=1,
  ActionGeo_CountryCode=c("IR", "IQ", "KW", "OM", "QA", "SA", "SY", "AE"))

gdelt <- GetGDELT(
  start.date="2000-01-01",
  end.date="2000-12-31",
  filter=country_code_filter,
  local.folder="./data/gdelt/")

saveRDS(gdelt, "gdelt_2000.rds")



gdelt2 <- GetGDELT(
  start.date="2014-03-20",
  end.date="2016-04-01",
  filter=country_code_filter,
  local.folder="./data/gdelt/")

saveRDS(gdelt2, "gdelt_2014.03.19_to_2016.04.01.rds")



new_gdelt <- gdelt[abs(gdelt$GoldsteinScale) >= 8.0,]
nodups_gdelt <- new_gdelt[!duplicated(new_gdelt[,c("DATEADDED", "ActionGeo_CountryCode", "EventCode", "SOURCEURL")]),]


new_gdelt$SOURCEURL
new_gdelt[579:580,]

table(gdelt$ActionGeo_ADM1Code)
table(gdelt$ActionGeo_CountryCode)
summary(gdelt)
## End(Not run)

unique(substr(gdelt$ActionGeo_ADM1Code, 0, 2))
unique(gdelt$ActionGeo_CountryCode)


gdelt$IsRootEvent
new_gdelt <- gdelt[abs(gdelt$GoldsteinScale) >= 8.0,]
gdelt
new_gdelt$GoldsteinScale
new_gdelt


# Download the entire post-20140319 GDELT database
#GetGDELT(start.date = "2014/03/20", 
#end.date = "2015/01/01", 
#local.folder = "./Data", 
#data.url.root = "http://data.gdeltproject.org/events/",
#verbose = TRUE)

# Option 1
# Doesn't work well
plot(oil_fateh, col="red")
lines(oil_can, col="blue")
lines(oil_henry, col="green")
lines(oil_opec, col="yellow")

# Option 2
# Works well
z <- na.approx(cbind(oil_fateh, oil_can, oil_henry, oil_opec))
plot.zoo(
  z, col=2:5,
  ylab = c("Fateh", "Canada", "Henry Hub Gas", "Opec"),
  main="Oil and Derivates")



diesel_ny_daily <- na.trim(xts(data5$New.York.Harbor.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Dollars.per.Gallon., as.Date(data5$Date, format="%b %d, %Y")))
saveRDS(diesel_ny_daily, "diesel_ny_daily")

diesel_us_gulf_coast_daily <- na.trim(xts(data5$U.S..Gulf.Coast.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Dollars.per.Gallon., as.Date(data5$Date, format="%b %d, %Y")))
saveRDS(diesel_us_gulf_coast_daily, "diesel_us_gulf_coast_daily")

diesel_us_la_daily <- na.trim(xts(data5$Los.Angeles..CA.Ultra.Low.Sulfur.CARB.Diesel.Spot.Price..Dollars.per.Gallon., as.Date(data5$Date, format="%b %d, %Y")))
saveRDS(diesel_us_la_daily, "diesel_us_la_daily")

kerosene_jet_fuel_daily <- na.trim(xts(data6$U.S..Gulf.Coast.Kerosene.Type.Jet.Fuel.Spot.Price.FOB..Dollars.per.Gallon., as.Date(data6$Date, format="%b %d, %Y")))
saveRDS(kerosene_jet_fuel_daily, "kerosene_jet_fuel_daily.rds")

heating_oil_ny_daily <- na.trim(xts(data4$New.York.Harbor.No..2.Heating.Oil.Spot.Price.FOB..Dollars.per.Gallon., as.Date(data4$Date, format="%b %d, %Y")))
saveRDS(heating_oil_ny_daily, "heating_oil_ny_daily.rds")

gasoline_us_la_daily <- na.trim(xts(data3$Los.Angeles.Reformulated.RBOB.Regular.Gasoline.Spot.Price..Dollars.per.Gallon., as.Date(data3$Date, format="%b %d, %Y")))
saveRDS(gasoline_us_la_daily, "gasoline_us_la_daily.rds")

gasoline_ny_daily <- na.trim(xts(data2$New.York.Harbor.Conventional.Gasoline.Regular.Spot.Price.FOB..Dollars.per.Gallon., as.Date(data2$Date, format="%b %d, %Y")))
saveRDS(gasoline_ny_daily, "gasoline_ny_daily.rds")

gasoline_us_gulf_cost_daily <- na.trim(xts(data2$U.S..Gulf.Coast.Conventional.Gasoline.Regular.Spot.Price.FOB..Dollars.per.Gallon., as.Date(data2$Date, format="%b %d, %Y")))
saveRDS(gasoline_us_gulf_cost_daily, "gasoline_us_gulf_cost_daily.rds")

oil_wti_daily <- na.trim(xts(data1$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., as.Date(data1$Date, format="%b %d, %Y")))
saveRDS(oil_wti_daily, "oil_wti_daily.rds")
oil_brent_daily <- na.trim(xts(data1$Europe.Brent.Spot.Price.FOB..Dollars.per.Barrel., as.Date(data1$Date, format="%b %d, %Y")))
saveRDS(oil_brent_daily, "oil_brent_daily.rds")

oil_wti_daily <- xts(data1$value1, as.Date(data1$Date, format="%b %d, %Y"))
oil_brent_daily <- xts(data1$value2, as.Date(data1$Date, format="%b %d, %Y"))

gdelt <- readRDS("./data/gdelt_2000.rds")

#as.Date(as.character(gdelt$SQLDATE), format="%Y%m%d")
gdelt$Date <- as.Date(as.character(gdelt$SQLDATE), format="%Y%m%d")
gdelt_imp_red <- gdelt[gdelt$EventRootCode==20, c("Date", "EventCode", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "ActionGeo_CountryCode", "SOURCEURL")]
a <- aggregate(gdelt_imp_red, by = list(gdelt_imp_red$Date), length)
gdelt_2000_event_count <- xts(a[,2],a[,1])
plot(gdelt_2000_event_count)
