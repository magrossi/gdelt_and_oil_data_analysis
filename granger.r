#
# CA683 - Assignment 2 - Granger Causality Analysis
#

# Packages Used
# install.packages("xts")
# install.packages("forecast")
# install.packages("lmtest")
# install.packages("vars")
# install.packages("MSBVAR")
require("xts")
require("forecast")
require("lmtest")
require("vars")
require("MSBVAR")

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

# Transform GDELT event counts into time series of 0 (no events) or 1 (events)
# without taking into consideration other event factions such as total count, avg tone, num mentions, etc.
gdelt_simple <- ifelse(gdelt[,1] > 0, 1, 0)
names(gdelt_simple) <- c("EventHappened")

# Merge oil and gdelt datasets for further analysis and normalize event column
# placing zeros (no event happened) on days where we had NA
oil_gdelt <- merge(gdelt_simple, oil_and_derivates_diff)
oil_gdelt[is.na(oil_gdelt[,1]),1] <- 0

plot.zoo(oil_gdelt, col=1:ncol(oil_gdelt), main="Differenciated Oil & Derivates\nGDELT Event Days", las=1)

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
    gt <- grangertest(g, o, order = i)
    res <- gt$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste("GDELT ->", colnames(o),"F:",gt$F[2]," Pr(>F):",res," Lag:",i,"\n"))
    }
    
    gt <- grangertest(o, g, order = i)
    res <- gt$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste(colnames(o),"-> GDELT   F:",gt$F[2]," Pr(>F):",res," Lag:",i,"\n"))
    }
  }
}

# ##########################################################################
# Results from p-0.01 and lags from 1 to 10
# ##########################################################################
# Gasoline.US.NY.Daily -> GDELT   F: 3.09877392999219  Pr(>F): 0.00849157917309497  Lag: 5 
# Oil.Brent.Daily -> GDELT   F: 2.94600323208041  Pr(>F): 0.00170437162976742  Lag: 9 
# Oil.Brent.Daily -> GDELT   F: 2.74088483536638  Pr(>F): 0.00227228278750451  Lag: 10 
# Oil.Canada.Monthly -> GDELT   F: 25.2393463209426  Pr(>F): 5.57554708412871e-07  Lag: 1 
# GDELT -> Oil.Canada.Monthly F: 5.0629049150379  Pr(>F): 0.00641940976419122  Lag: 2 
# Oil.Canada.Monthly -> GDELT   F: 8.89973178492887  Pr(>F): 0.000142647979962646  Lag: 2 
# Oil.Canada.Monthly -> GDELT   F: 4.51867609245534  Pr(>F): 0.0036575144811257  Lag: 3 
# GDELT -> Oil.Fateh.Monthly F: 7.78153753640789  Pr(>F): 0.005290820976944  Lag: 1 
# Oil.Fateh.Monthly -> GDELT   F: 6.75547826028844  Pr(>F): 0.00936325591856004  Lag: 1 
# Oil.WTI.Daily -> GDELT   F: 2.90169333045059  Pr(>F): 0.00793490123909961  Lag: 6 
# Oil.WTI.Daily -> GDELT   F: 2.69246507167141  Pr(>F): 0.00398762776270035  Lag: 9 
# Oil.WTI.Daily -> GDELT   F: 2.41874561730178  Pr(>F): 0.00718018580046149  Lag: 10 
# Propane.Mont.Belvieu.Daily -> GDELT   F: 2.7992936106301  Pr(>F): 0.00657227331876838  Lag: 7 
# Propane.Mont.Belvieu.Daily -> GDELT   F: 3.05469478596116  Pr(>F): 0.00117940830223101  Lag: 9 
# Propane.Mont.Belvieu.Daily -> GDELT   F: 3.49292022657247  Pr(>F): 0.000132265457902877  Lag: 10 
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
#                                     F-statistic      p-value lag
# Gasoline.US.NY.Daily -> GDELT          3.098774 8.491579e-03   5
# Oil.Brent.Daily -> GDELT               2.946003 1.704372e-03   9
# Oil.Brent.Daily -> GDELT               2.740885 2.272283e-03  10
# Oil.Canada.Monthly -> GDELT           25.239346 5.575547e-07   1
# Oil.Canada.Monthly -> GDELT            8.899732 1.426480e-04   2
# GDELT -> Oil.Canada.Monthly            5.062905 6.419410e-03   2
# Oil.Canada.Monthly -> GDELT            4.518676 3.657514e-03   3
# Oil.Fateh.Monthly -> GDELT             6.755478 9.363256e-03   1
# GDELT -> Oil.Fateh.Monthly             7.781538 5.290821e-03   1
# Oil.WTI.Daily -> GDELT                 2.901693 7.934901e-03   6
# Oil.WTI.Daily -> GDELT                 2.692465 3.987628e-03   9
# Oil.WTI.Daily -> GDELT                 2.418746 7.180186e-03  10
# Propane.Mont.Belvieu.Daily -> GDELT    2.799294 6.572273e-03   7
# Propane.Mont.Belvieu.Daily -> GDELT    3.054695 1.179408e-03   9
# Propane.Mont.Belvieu.Daily -> GDELT    3.492920 1.322655e-04  10
# ##########################################################################