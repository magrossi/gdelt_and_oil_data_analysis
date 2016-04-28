require("xts")
require("forecast")
require("lmtest")
require("vars")
require("MSBVAR")
library(forecast)

gdelt <- readRDS("./data/gdelt_indicators.rds")

gdelt$EventCount <- ts(gdelt$EventCount,start = c(2000,1), end = c(2016,1),frequency = 1)
summary(gdelt)
event <- diff(gdelt$EventCount,4)
ev <- arima(event, order = c(1,0,0))

oil_and_derivates$Oil.Fateh.Monthly <- ts(oil_and_derivates$Oil.Fateh.Monthly,start = c(2000,1), end = c(2016,1),frequency = 1)
Fateh <- diff(oil_and_derivates$Oil.Fateh.Monthly,4)
eve <- arima(Fateh, order = c(2,0,0))
plot(eve)


oil_and_derivates$Diesel.US.NY.Daily <- ts(oil_and_derivates$Diesel.US.NY.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Diesel <- diff(oil_and_derivates$Diesel.US.NY.Daily,4)
Die <- arima(Diesel, order = c(2,0,0))
plot(Die)

oil_and_derivates$Diesel.US.Gulf.Coast.Daily <- ts(oil_and_derivates$Diesel.US.Gulf.Coast.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Diesel.Gulf <- diff(oil_and_derivates$Diesel.US.Gulf.Coast.Daily,4)
Diesel.GulfAR <- arima(Diesel.Gulf, order = c(2,0,0))
plot(Diesel.GulfAR)

oil_and_derivates$Diesel.US.LA.Daily <- ts(oil_and_derivates$Diesel.US.LA.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Diesel.LA <- diff(oil_and_derivates$Diesel.US.LA.Daily,4)
Diesel.LAarima <- arima(Diesel.LA, order = c(2,0,0))

oil_and_derivates$Gasoline.US.NY.Daily <- ts(oil_and_derivates$Gasoline.US.NY.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Gasoline.NY <- diff(oil_and_derivates$Gasoline.US.NY.Daily,4)
Gasoline.NYarima <- arima(Gasoline.NY, order = c(2,0,0))

oil_and_derivates$Gasoline.US.Gulf.Coast.Daily <- ts(oil_and_derivates$Gasoline.US.Gulf.Coast.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Gasoline.Gulf <- diff(oil_and_derivates$Gasoline.US.Gulf.Coast.Daily,4)
Gasoline.Gulfarima <- arima(Gasoline.Gulf, order = c(2,0,0))

oil_and_derivates$Gasoline.US.LA.Daily <- ts(oil_and_derivates$Gasoline.US.LA.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Gasoline.LA <- diff(oil_and_derivates$Gasoline.US.LA.Daily,4)
Gasoline.LAarima <- arima(Gasoline.LA, order = c(2,0,0))

oil_and_derivates$Heating.Oil.US.NY.Daily <- ts(oil_and_derivates$Heating.Oil.US.NY.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Heating.oil <- diff(oil_and_derivates$Heating.Oil.US.NY.Daily,4)
Heating.oilarima <- arima(Heating.oil, order = c(3,0,0))

oil_and_derivates$Henry.Hub.Natural.Gas.Daily <- ts(oil_and_derivates$Henry.Hub.Natural.Gas.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Natural.Gas <- diff(oil_and_derivates$Henry.Hub.Natural.Gas.Daily,4)
Natural.Gasarima <- arima(Natural.Gas, order = c(3,0,0))

oil_and_derivates$Kerosene.Jet.Fuel.Daily <- ts(oil_and_derivates$Kerosene.Jet.Fuel.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Kerosene.Jet <- diff(oil_and_derivates$Kerosene.Jet.Fuel.Daily,4)
Kerosene.Jetarima <- arima(Kerosene.Jet, order = c(2,0,0))

oil_and_derivates$Oil.Brent.Daily <- ts(oil_and_derivates$Oil.Brent.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Oil.Brent <- diff(oil_and_derivates$Oil.Brent.Daily,4)
Oil.Brentarima <- arima(Oil.Brent, order = c(2,0,0))

oil_and_derivates$Oil.Canada.Monthly <- ts(oil_and_derivates$Oil.Canada.Monthly,start = c(2000,1), end = c(2016,1),frequency = 1)
Oil.Canada <- diff(oil_and_derivates$Oil.Canada.Monthly,4)
Oil.Canadaarima <- arima(Oil.Canada, order = c(2,0,0))

oil_and_derivates$Oil.OPEC.Daily <- ts(oil_and_derivates$Oil.OPEC.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Oil.OPEC <- diff(oil_and_derivates$Oil.OPEC.Daily,4)
Oil.OPECarima <- arima(Oil.OPEC, order = c(2,0,0))

oil_and_derivates$Oil.WTI.Daily <- ts(oil_and_derivates$Oil.WTI.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Oil.WTI <- diff(oil_and_derivates$Oil.WTI.Daily,4)
Oil.WTIarima <- arima(Oil.WTI, order = c(2,0,0))

oil_and_derivates$Propane.Mont.Belvieu.Daily <- ts(oil_and_derivates$Propane.Mont.Belvieu.Daily,start = c(2000,1), end = c(2016,1),frequency = 1)
Propane.Mont <- diff(oil_and_derivates$Propane.Mont.Belvieu.Daily,4)
Propane.Montarima <- arima(Propane.Mont, order = c(2,0,0))



