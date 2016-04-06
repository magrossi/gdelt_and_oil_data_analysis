#
# CA683 - Assignment 2 - Granger Causality Analysis
#

# Testing for Granger-causality between GDELT Normalized Weighted Event Counts from the target Countries
# In Oil & Derivatives Prices
# The event counts are weighted by Number Of Mentions what creates the calculated "GDELT Index" figure

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
      cat(paste("GDELT ->", colnames(o)," Lag:",i,"Pr(>F):",res,"\n"))
    }

    res <- grangertest(o, g, order = i)$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste(colnames(o),"-> GDELT   Lag:",i,"Pr(>F):",res,"\n"))
    }
  }
}

# ##########################################################################
# Results from p-0.01 and lags from 1 to 10
# ##########################################################################
# Diesel.US.LA.Daily -> GDELT   Lag: 1 Pr(>F): 0.00514135449661481 
# GDELT -> Diesel.US.LA.Daily  Lag: 7 Pr(>F): 0.00823951926676257 
# Gasoline.US.NY.Daily -> GDELT   Lag: 5 Pr(>F): 0.00564856199650821 
# Gasoline.US.NY.Daily -> GDELT   Lag: 6 Pr(>F): 0.00515834073170076 
# Gasoline.US.NY.Daily -> GDELT   Lag: 7 Pr(>F): 0.00994801884716731 
# Gasoline.US.NY.Daily -> GDELT   Lag: 9 Pr(>F): 0.00740479818154645 
# GDELT -> Gasoline.US.LA.Daily  Lag: 9 Pr(>F): 0.00116041475095885 
# GDELT -> Gasoline.US.LA.Daily  Lag: 10 Pr(>F): 0.000158343453785841 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 1 Pr(>F): 0.00315042621444897 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 5 Pr(>F): 0.00882957503392254 
# GDELT -> Heating.Oil.US.NY.Daily  Lag: 7 Pr(>F): 0.00224264233659656 
# GDELT -> Heating.Oil.US.NY.Daily  Lag: 8 Pr(>F): 0.00425132502545885 
# GDELT -> Heating.Oil.US.NY.Daily  Lag: 9 Pr(>F): 0.00595977867188916 
# Heating.Oil.US.NY.Daily -> GDELT   Lag: 9 Pr(>F): 0.00650100542730044 
# GDELT -> Oil.Brent.Daily  Lag: 1 Pr(>F): 0.00152124188594869 
# Oil.Brent.Daily -> GDELT   Lag: 1 Pr(>F): 0.00159059626913838 
# GDELT -> Oil.Brent.Daily  Lag: 2 Pr(>F): 0.00599087559643419 
# GDELT -> Oil.Brent.Daily  Lag: 3 Pr(>F): 0.00157814407370973 
# GDELT -> Oil.Brent.Daily  Lag: 4 Pr(>F): 0.00410610708761312 
# GDELT -> Oil.Brent.Daily  Lag: 5 Pr(>F): 0.00012916604657337 
# GDELT -> Oil.Brent.Daily  Lag: 6 Pr(>F): 4.45263681360242e-05 
# GDELT -> Oil.Brent.Daily  Lag: 7 Pr(>F): 0.000126264769190623 
# GDELT -> Oil.Brent.Daily  Lag: 8 Pr(>F): 0.000184528463219038 
# GDELT -> Oil.Brent.Daily  Lag: 9 Pr(>F): 0.000308998290055237 
# GDELT -> Oil.Brent.Daily  Lag: 10 Pr(>F): 0.00051287592824948 
# GDELT -> Oil.Fateh.Monthly  Lag: 1 Pr(>F): 0.000865514951312661 
# Oil.Fateh.Monthly -> GDELT   Lag: 1 Pr(>F): 0.000366124090434988 
# GDELT -> Oil.Fateh.Monthly  Lag: 2 Pr(>F): 0.00753890946326704 
# Oil.Fateh.Monthly -> GDELT   Lag: 2 Pr(>F): 0.00409597486827836 
# Oil.WTI.Daily -> GDELT   Lag: 1 Pr(>F): 0.00156889151336203 
# GDELT -> Oil.WTI.Daily  Lag: 5 Pr(>F): 1.03795835298342e-05 
# Oil.WTI.Daily -> GDELT   Lag: 5 Pr(>F): 0.00558382578834927 
# GDELT -> Oil.WTI.Daily  Lag: 6 Pr(>F): 1.14903202944918e-07 
# Oil.WTI.Daily -> GDELT   Lag: 6 Pr(>F): 0.00968224645947826 
# GDELT -> Oil.WTI.Daily  Lag: 7 Pr(>F): 9.69923929515621e-08 
# GDELT -> Oil.WTI.Daily  Lag: 8 Pr(>F): 2.25965040775542e-07 
# GDELT -> Oil.WTI.Daily  Lag: 9 Pr(>F): 3.77747237143026e-07 
# Oil.WTI.Daily -> GDELT   Lag: 9 Pr(>F): 0.00260369992247111 
# GDELT -> Oil.WTI.Daily  Lag: 10 Pr(>F): 9.00800229287868e-07 
# Oil.WTI.Daily -> GDELT   Lag: 10 Pr(>F): 0.0062038521915368 
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
#                                  F-statistic      p-value lag
# Diesel.US.LA.Daily -> GDELT         7.835656 5.141354e-03   1
# GDELT -> Diesel.US.LA.Daily         2.715559 8.239519e-03   7
# Gasoline.US.NY.Daily -> GDELT       3.294665 5.648562e-03   5
# Gasoline.US.NY.Daily -> GDELT       3.081247 5.158341e-03   6
# Gasoline.US.NY.Daily -> GDELT       2.643582 9.948019e-03   7
# Gasoline.US.NY.Daily -> GDELT       2.503231 7.404798e-03   9
# GDELT -> Gasoline.US.LA.Daily       3.063573 1.160415e-03   9
# GDELT -> Gasoline.US.LA.Daily       3.452796 1.583435e-04  10
# Heating.Oil.US.NY.Daily -> GDELT    8.723673 3.150426e-03   1
# Heating.Oil.US.NY.Daily -> GDELT    3.079907 8.829575e-03   5
# GDELT -> Heating.Oil.US.NY.Daily    3.191202 2.242642e-03   7
# GDELT -> Heating.Oil.US.NY.Daily    2.801005 4.251325e-03   8
# Heating.Oil.US.NY.Daily -> GDELT    2.543421 6.501005e-03   9
# GDELT -> Heating.Oil.US.NY.Daily    2.570140 5.959779e-03   9
# Oil.Brent.Daily -> GDELT            9.977849 1.590596e-03   1
# GDELT -> Oil.Brent.Daily           10.060071 1.521242e-03   1
# GDELT -> Oil.Brent.Daily            5.120990 5.990876e-03   2
# GDELT -> Oil.Brent.Daily            5.104513 1.578144e-03   3
# GDELT -> Oil.Brent.Daily            3.829983 4.106107e-03   4
# GDELT -> Oil.Brent.Daily            5.041484 1.291660e-04   5
# GDELT -> Oil.Brent.Daily            4.960974 4.452637e-05   6
# GDELT -> Oil.Brent.Daily            4.195921 1.262648e-04   7
# GDELT -> Oil.Brent.Daily            3.797805 1.845285e-04   8
# GDELT -> Oil.Brent.Daily            3.437834 3.089983e-04   9
# GDELT -> Oil.Brent.Daily            3.140189 5.128759e-04  10
# Oil.Fateh.Monthly -> GDELT         12.708526 3.661241e-04   1
# GDELT -> Oil.Fateh.Monthly         11.103625 8.655150e-04   1
# Oil.Fateh.Monthly -> GDELT          5.501547 4.095975e-03   2
# GDELT -> Oil.Fateh.Monthly          4.890678 7.538909e-03   2
# Oil.WTI.Daily -> GDELT             10.002859 1.568892e-03   1
# Oil.WTI.Daily -> GDELT              3.300136 5.583826e-03   5
# GDELT -> Oil.WTI.Daily              6.165685 1.037958e-05   5
# Oil.WTI.Daily -> GDELT              2.817907 9.682246e-03   6
# GDELT -> Oil.WTI.Daily              7.189942 1.149032e-07   6
# GDELT -> Oil.WTI.Daily              6.555222 9.699239e-08   7
# GDELT -> Oil.WTI.Daily              5.779150 2.259650e-07   8
# Oil.WTI.Daily -> GDELT              2.820379 2.603700e-03   9
# GDELT -> Oil.WTI.Daily              5.245457 3.777472e-07   9
# Oil.WTI.Daily -> GDELT              2.460462 6.203852e-03  10
# GDELT -> Oil.WTI.Daily              4.722969 9.008002e-07  10
# ##########################################################################