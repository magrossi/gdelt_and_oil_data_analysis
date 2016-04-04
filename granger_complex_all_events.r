#
# CA683 - Assignment 2 - Granger Causality Analysis
#

# Packages Used
# install.packages("xts")
# install.packages("forecast")
# install.packages("lmtest")
# install.packages("vars")
# install.packages("MSBVAR")
# install.packages("TTR")
require("xts")
require("forecast")
require("lmtest")
require("vars")
require("MSBVAR")
require("TTR")

# Oil and Derivatives
oil_and_derivates <- readRDS("./data/oil_and_derivates.rds")

# Differenciate oils data to make sure stationary
oil_and_derivates_diff <- NULL
for (i in 1:length(names(oil_and_derivates))) {
  oil_and_derivates_diff <- cbind(oil_and_derivates_diff, diff(oil_and_derivates[,i],ndiffs(oil_and_derivates[,i], alpha=0.05, test=c("kpss"))))
}

# Load the total number of events by country to normalize for the exponential growth in
# new availability
gdelt_event_by_country <- readRDS("./data/gdelt_daily_counts_all_events_all_countries.rds")
gdelt_event_by_day <- aggregate(gdelt_event_by_country[,c("Count")], by=list(gdelt_event_by_country$Date), sum, na.rm=TRUE)
gdelt_event_by_day <- xts(gdelt_event_by_day$x, gdelt_event_by_day$Group.1)

# Smooth by a 10-year simple moving average to get rid of all the noise
# Then normalize to a 0..1 interval
gdelt_event_by_day_smooth <- SMA(gdelt_event_by_day, n=3650)
gdelt_event_by_day_smooth <- gdelt_event_by_day_smooth/max(gdelt_event_by_day_smooth, na.rm=TRUE)
plot.zoo(gdelt_event_by_day_smooth)

# Do the same again but only aggregate by the relevant countries we are interested in
gdelt_countries <- gdelt_event_by_country[gdelt_event_by_country$CountryCode %in% c("IR", "IQ", "KW", "OM", "QA", "SA", "SY", "AE"),]
gdelt_countries_by_day <- aggregate(gdelt_countries[,c("Count")], by=list(gdelt_countries$Date), sum, na.rm=TRUE)
gdelt_countries_by_day <- xts(gdelt_countries_by_day$x, gdelt_countries_by_day$Group.1)

# Use the normalized daily event count as the normal basis for the gdelt selected events like:
# GDELT Event Count * GDELT Number of Mentions / Daily Event Normalization
gdelt_normal <- merge(gdelt_countries_by_day,gdelt_event_by_day_smooth*100,all=FALSE)
gdelt_normal <- gdelt_normal[complete.cases(gdelt_normal),]
gdelt_normal <- gdelt_normal[,1]/gdelt_normal[,2]
colnames(gdelt_normal) <- c("GDELT Count")
plot.zoo(gdelt_normal)

# Merge oil and gdelt datasets for further analysis
oil_gdelt <- merge(gdelt_normal, oil_and_derivates_diff)
oil_gdelt[is.na(oil_gdelt[,1]),1] <- 0

plot.zoo(oil_gdelt, col=1:ncol(oil_gdelt), main="Differenciated Oil & Derivates\nGDELT Count", las=1)

# ##########################################################################
# Method 1: lmtest
# http://www.r-bloggers.com/chicken-or-the-egg-granger-causality-for-the-masses/
# ##########################################################################
pval <- 0.01
max_order <- 10
for (j in 2:ncol(oil_gdelt)) {
  m <- merge(oil_gdelt[,1], oil_gdelt[,j], all=FALSE)
  m <- m[complete.cases(m),]
  g <- m[,1]
  o <- m[,2]
  for (i in 1:max_order) {
    res <- grangertest(g, o, order = i)$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste(colnames(o),"-> GDELT   Lag:",i,"Pr(>F):",res,"\n"))
    }

    res <- grangertest(o, g, order = i)$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste("GDELT ->", colnames(o)," Lag:",i,"Pr(>F):",res,"\n"))
    }
  }
}

# ##########################################################################
# Results from p-0.01 and lags from 1 to 10
# ##########################################################################
# Gasoline.US.NY.Daily -> GDELT   Lag: 4 Pr(>F): 0.000783229258605881 
# Gasoline.US.NY.Daily -> GDELT   Lag: 5 Pr(>F): 0.000428016443951224 
# Gasoline.US.NY.Daily -> GDELT   Lag: 6 Pr(>F): 0.000223348991602642 
# Gasoline.US.NY.Daily -> GDELT   Lag: 7 Pr(>F): 0.00063773448388498 
# Gasoline.US.NY.Daily -> GDELT   Lag: 8 Pr(>F): 0.000627619638613398 
# Gasoline.US.NY.Daily -> GDELT   Lag: 9 Pr(>F): 0.00102466492653061 
# Gasoline.US.NY.Daily -> GDELT   Lag: 10 Pr(>F): 0.00234230846313214 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 3 Pr(>F): 0.00321683212376121 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 4 Pr(>F): 0.000756213097918563 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 5 Pr(>F): 0.000291070144720741 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 6 Pr(>F): 0.000154679774680858 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 7 Pr(>F): 0.000436783690989877 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 8 Pr(>F): 0.000880523201176778 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 9 Pr(>F): 0.0011863577421909 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 10 Pr(>F): 0.00167589966854365 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 4 Pr(>F): 0.00869143289285661 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 6 Pr(>F): 0.00193271804672975 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 7 Pr(>F): 0.00242657082615494 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 8 Pr(>F): 0.00271212834078141 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 9 Pr(>F): 0.000271638562834226 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 10 Pr(>F): 0.000533280716153947 
# Henry.Hub.Natural.Gas.Daily -> GDELT   Lag: 10 Pr(>F): 0.00571326103191461 
# Kerosene.Jet.Fuel.Daily -> GDELT   Lag: 4 Pr(>F): 0.00439255292112465 
# Kerosene.Jet.Fuel.Daily -> GDELT   Lag: 5 Pr(>F): 0.00924416916913057 
# ##########################################################################

# ##########################################################################
# Method 2: MSBVAR
# http://www.inside-r.org/packages/cran/MSBVAR/docs/granger.test
# ##########################################################################
res <- NULL
for (i in 2:ncol(oil_gdelt)) {
  m <- merge(oil_gdelt[,1], oil_gdelt[,i], all=FALSE)
  m <- m[complete.cases(m),]
  colnames(m) <- c("GDELT", colnames(oil_gdelt[,i]))
  for (j in 1:max_order) {
    g <- cbind(granger.test(m, j), lag = j)
    res <- rbind(res, g)
  }
}
res <- res[res[,2] < pval,]
print(res)

# ###########################################################################
# Results from p-0.01 and lags from 1 to 10
# ###########################################################################                                       F-statistic      p-value lag
#                                       F-statistic      p-value lag
# GDELT -> Gasoline.US.NY.Daily            4.757105 0.0007832293   4
# GDELT -> Gasoline.US.NY.Daily            4.497595 0.0004280164   5
# GDELT -> Gasoline.US.NY.Daily            4.338229 0.0002233490   6
# GDELT -> Gasoline.US.NY.Daily            3.636929 0.0006377345   7
# GDELT -> Gasoline.US.NY.Daily            3.417066 0.0006276196   8
# GDELT -> Gasoline.US.NY.Daily            3.094556 0.0010246649   9
# GDELT -> Gasoline.US.NY.Daily            2.732449 0.0023423085  10
# GDELT -> Gasoline.US.Gulf.Coast.Daily    4.597901 0.0032168321   3
# GDELT -> Gasoline.US.Gulf.Coast.Daily    4.776544 0.0007562131   4
# GDELT -> Gasoline.US.Gulf.Coast.Daily    4.673375 0.0002910701   5
# GDELT -> Gasoline.US.Gulf.Coast.Daily    4.481082 0.0001546798   6
# GDELT -> Gasoline.US.Gulf.Coast.Daily    3.768864 0.0004367837   7
# GDELT -> Gasoline.US.Gulf.Coast.Daily    3.310170 0.0008805232   8
# GDELT -> Gasoline.US.Gulf.Coast.Daily    3.051951 0.0011863577   9
# GDELT -> Gasoline.US.Gulf.Coast.Daily    2.823813 0.0016758997  10
# GDELT -> Heating.Oil.US.NY.Daily         3.402270 0.0086914329   4
# GDELT -> Heating.Oil.US.NY.Daily         3.482776 0.0019327180   6
# GDELT -> Heating.Oil.US.NY.Daily         3.162825 0.0024265708   7
# GDELT -> Heating.Oil.US.NY.Daily         2.948670 0.0027121283   8
# GDELT -> Heating.Oil.US.NY.Daily         3.474023 0.0002716386   9
# GDELT -> Heating.Oil.US.NY.Daily         3.129767 0.0005332807  10
# GDELT -> Henry.Hub.Natural.Gas.Daily     2.485379 0.0057132610  10
# GDELT -> Kerosene.Jet.Fuel.Daily         3.792131 0.0043925529   4
# GDELT -> Kerosene.Jet.Fuel.Daily         3.058055 0.0092441692   5
# ##########################################################################