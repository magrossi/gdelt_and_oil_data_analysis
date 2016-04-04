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

# Read baseline data
# GDELT Event Counts
gdelt <- readRDS("./data/gdelt_indicators.rds")

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

# Use the normalized daily event count as the normal basis for the gdelt selected events like:
# GDELT Event Count * GDELT Number of Mentions / Daily Event Normalization
gdelt_normal <- merge(gdelt,gdelt_event_by_day_smooth,all=FALSE)
gdelt_normal <- gdelt_normal[,1]*gdelt_normal[,3]/gdelt_normal[,6]
gdelt_normal <- na.trim(gdelt_normal[,1])
colnames(gdelt_normal) <- c("GDELT Index")
plot.zoo(gdelt_normal)

# Merge oil and gdelt datasets for further analysis
oil_gdelt <- merge(gdelt_normal, oil_and_derivates_diff)
oil_gdelt[is.na(oil_gdelt[,1]),1] <- 0

plot.zoo(oil_gdelt, col=1:ncol(oil_gdelt), main="Differenciated Oil & Derivates\nGDELT Index", las=1)

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
# Gasoline.US.NY.Daily -> GDELT   Lag: 4 Pr(>F): 0.00588372863050341 
# Gasoline.US.NY.Daily -> GDELT   Lag: 7 Pr(>F): 0.00734249422412508 
# Gasoline.US.Gulf.Coast.Daily -> GDELT   Lag: 2 Pr(>F): 0.00501948275073459 
# Gasoline.US.LA.Daily -> GDELT   Lag: 10 Pr(>F): 0.00916178317231255 
# Oil.Brent.Daily -> GDELT   Lag: 2 Pr(>F): 0.00215790951308497 
# Oil.Brent.Daily -> GDELT   Lag: 3 Pr(>F): 0.00625657395413117 
# Oil.Brent.Daily -> GDELT   Lag: 5 Pr(>F): 0.00662115204892865 
# Oil.Brent.Daily -> GDELT   Lag: 6 Pr(>F): 0.00278162676534521 
# Oil.Brent.Daily -> GDELT   Lag: 7 Pr(>F): 0.00343808022987204 
# Oil.Brent.Daily -> GDELT   Lag: 8 Pr(>F): 0.00268963693405747 
# Oil.Brent.Daily -> GDELT   Lag: 9 Pr(>F): 0.00496169757629982 
# Oil.Brent.Daily -> GDELT   Lag: 10 Pr(>F): 0.00236610743799424 
# GDELT -> Oil.Canada.Monthly  Lag: 1 Pr(>F): 0.00334385135606773 
# Oil.Fateh.Monthly -> GDELT   Lag: 1 Pr(>F): 0.00436747574905057 
# GDELT -> Oil.Fateh.Monthly  Lag: 1 Pr(>F): 0.000863347979109852 
# GDELT -> Oil.Fateh.Monthly  Lag: 2 Pr(>F): 0.00704429555010789 
# Oil.OPEC.Daily -> GDELT   Lag: 2 Pr(>F): 0.00721852045013533 
# Oil.WTI.Daily -> GDELT   Lag: 2 Pr(>F): 0.000954292301946692 
# Oil.WTI.Daily -> GDELT   Lag: 3 Pr(>F): 0.00249427189735466 
# Oil.WTI.Daily -> GDELT   Lag: 4 Pr(>F): 0.00567801549630374 
# Oil.WTI.Daily -> GDELT   Lag: 5 Pr(>F): 0.00321169096799687 
# GDELT -> Oil.WTI.Daily  Lag: 5 Pr(>F): 0.00466729124235263 
# Oil.WTI.Daily -> GDELT   Lag: 6 Pr(>F): 0.00189845774166305 
# GDELT -> Oil.WTI.Daily  Lag: 6 Pr(>F): 0.00853579103548047 
# Oil.WTI.Daily -> GDELT   Lag: 7 Pr(>F): 0.00148983558882053 
# Oil.WTI.Daily -> GDELT   Lag: 8 Pr(>F): 0.00286038266445096 
# Oil.WTI.Daily -> GDELT   Lag: 9 Pr(>F): 0.00448921148624171 
# GDELT -> Oil.WTI.Daily  Lag: 9 Pr(>F): 0.00478376324529098 
# Oil.WTI.Daily -> GDELT   Lag: 10 Pr(>F): 0.00755533316119919 
# GDELT -> Oil.WTI.Daily  Lag: 10 Pr(>F): 0.00808558750604931 
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
# GDELT -> Gasoline.US.NY.Daily            3.625504 0.0058837286   4
# GDELT -> Gasoline.US.NY.Daily            2.757399 0.0073424942   7
# GDELT -> Gasoline.US.Gulf.Coast.Daily    5.298027 0.0050194828   2
# GDELT -> Gasoline.US.LA.Daily            2.351609 0.0091617832  10
# GDELT -> Oil.Brent.Daily                 6.143612 0.0021579095   2
# GDELT -> Oil.Brent.Daily                 4.121935 0.0062565740   3
# GDELT -> Oil.Brent.Daily                 3.218661 0.0066211520   5
# GDELT -> Oil.Brent.Daily                 3.335078 0.0027816268   6
# GDELT -> Oil.Brent.Daily                 3.036779 0.0034380802   7
# GDELT -> Oil.Brent.Daily                 2.951496 0.0026896369   8
# GDELT -> Oil.Brent.Daily                 2.626273 0.0049616976   9
# GDELT -> Oil.Brent.Daily                 2.729786 0.0023661074  10
# Oil.Canada.Monthly -> GDELT              8.633114 0.0033438514   1
# Oil.Fateh.Monthly -> GDELT              11.108280 0.0008633480   1
# GDELT -> Oil.Fateh.Monthly               8.128999 0.0043674757   1
# Oil.Fateh.Monthly -> GDELT               4.958622 0.0070442956   2
# GDELT -> Oil.OPEC.Daily                  4.938113 0.0072185205   2
# GDELT -> Oil.WTI.Daily                   6.960668 0.0009542923   2
# GDELT -> Oil.WTI.Daily                   4.779111 0.0024942719   3
# GDELT -> Oil.WTI.Daily                   3.645744 0.0056780155   4
# Oil.WTI.Daily -> GDELT                   3.385635 0.0046672912   5
# GDELT -> Oil.WTI.Daily                   3.562768 0.0032116910   5
# Oil.WTI.Daily -> GDELT                   2.871026 0.0085357910   6
# GDELT -> Oil.WTI.Daily                   3.489956 0.0018984577   6
# GDELT -> Oil.WTI.Daily                   3.337495 0.0014898356   7
# GDELT -> Oil.WTI.Daily                   2.931249 0.0028603827   8
# Oil.WTI.Daily -> GDELT                   2.637263 0.0047837632   9
# GDELT -> Oil.WTI.Daily                   2.656577 0.0044892115   9
# Oil.WTI.Daily -> GDELT                   2.384650 0.0080855875  10
# GDELT -> Oil.WTI.Daily                   2.404146 0.0075553332  10
# ##########################################################################