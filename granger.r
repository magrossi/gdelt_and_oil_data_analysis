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
  for (i in 1:max_order) {
    res <- grangertest(oil_gdelt[,1], oil_gdelt[,j], order = i)$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste(colnames(oil_gdelt)[j],"-> GDELT   Lag:",i,"Pr(>F):",res,"\n"))
    }

    res <- grangertest(oil_gdelt[,j], oil_gdelt[,1], order = i)$`Pr(>F)`[2]
    if (res < pval) {
      cat(paste("GDELT ->", colnames(oil_gdelt)[j]," Lag:",i,"Pr(>F):",res,"\n"))
    }
  }
}

# ##########################################################################
# Results from p-0.01 and lags from 1 to 10
# ##########################################################################
#GDELT -> Gasoline.US.LA.Daily  Lag: 9 Pr(>F): 0.00618222537321311 
#GDELT -> Gasoline.US.LA.Daily  Lag: 10 Pr(>F): 0.00828675485591316 
#GDELT -> Heating.Oil.US.NY.Daily  Lag: 2 Pr(>F): 0.00986002845638264 
#GDELT -> Heating.Oil.US.NY.Daily  Lag: 3 Pr(>F): 0.00189462358250495 
#GDELT -> Heating.Oil.US.NY.Daily  Lag: 4 Pr(>F): 0.00955558030774416 
#GDELT -> Oil.Brent.Daily  Lag: 4 Pr(>F): 0.00320392410758377 
#Oil.Brent.Daily -> GDELT   Lag: 5 Pr(>F): 0.000525648439357657 
#GDELT -> Oil.Brent.Daily  Lag: 5 Pr(>F): 0.000134676050369996 
#Oil.Brent.Daily -> GDELT   Lag: 6 Pr(>F): 0.00239694746367075 
#GDELT -> Oil.Brent.Daily  Lag: 6 Pr(>F): 3.59564233692886e-05 
#Oil.Brent.Daily -> GDELT   Lag: 7 Pr(>F): 0.00174260919125085 
#GDELT -> Oil.Brent.Daily  Lag: 7 Pr(>F): 0.000652040658884683 
#Oil.Brent.Daily -> GDELT   Lag: 8 Pr(>F): 0.00469381927817036 
#GDELT -> Oil.Brent.Daily  Lag: 8 Pr(>F): 0.00129297521221249 
#Oil.Brent.Daily -> GDELT   Lag: 9 Pr(>F): 0.00673110992972709 
#GDELT -> Oil.Brent.Daily  Lag: 9 Pr(>F): 0.000684572483267351 
#Oil.Brent.Daily -> GDELT   Lag: 10 Pr(>F): 0.00382124021036716 
#GDELT -> Oil.Brent.Daily  Lag: 10 Pr(>F): 8.43005879932655e-06 
#GDELT -> Oil.Canada.Monthly  Lag: 1 Pr(>F): 2.52990662389485e-07 
#Oil.Canada.Monthly -> GDELT   Lag: 2 Pr(>F): 8.87687458645952e-06 
#GDELT -> Oil.Canada.Monthly  Lag: 2 Pr(>F): 0.00132123690598239 
#Oil.Canada.Monthly -> GDELT   Lag: 3 Pr(>F): 0.00193479642049843 
#GDELT -> Oil.WTI.Daily  Lag: 2 Pr(>F): 0.00760916037797443 
#GDELT -> Oil.WTI.Daily  Lag: 5 Pr(>F): 0.00298238616458016 
#GDELT -> Oil.WTI.Daily  Lag: 6 Pr(>F): 0.00796515565644134 
#Oil.WTI.Daily -> GDELT   Lag: 7 Pr(>F): 0.00364788637283011 
#GDELT -> Oil.WTI.Daily  Lag: 7 Pr(>F): 0.0056813339210181 
#Oil.WTI.Daily -> GDELT   Lag: 8 Pr(>F): 0.00279695847760343 
#GDELT -> Oil.WTI.Daily  Lag: 8 Pr(>F): 0.00686058805059049 
#GDELT -> Oil.WTI.Daily  Lag: 9 Pr(>F): 0.00611645274045129 
#GDELT -> Oil.WTI.Daily  Lag: 10 Pr(>F): 0.000990820824776688 
#GDELT -> Propane.Mont.Belvieu.Daily  Lag: 5 Pr(>F): 0.005982855537654 
#GDELT -> Propane.Mont.Belvieu.Daily  Lag: 10 Pr(>F): 0.00496006915196973 
# ##########################################################################

# ##########################################################################
# Method 2: MSBVAR
# http://www.inside-r.org/packages/cran/MSBVAR/docs/granger.test
# ##########################################################################
res <- NULL
for (i in 2:ncol(oil_gdelt)) {
  for (j in 1:max_order) {
    y <- cbind(oil_gdelt[,1], oil_gdelt[,i])
    colnames(y) <- c("GDELT", colnames(oil_gdelt[,i]))
    g <- cbind(granger.test(y, j), lag = j)
    g <- g[]
    res <- rbind(res, g)
  }
}
res <- res[res[,2] < pval,]
print(res)

# ##########################################################################
# Results from p-0.01 and lags from 1 to 10
# ###########################################################################                                       F-statistic      p-value lag
# Diesel.US.NY.Daily -> GDELT           3787.889945 0.000000e+00   1
# Diesel.US.NY.Daily -> GDELT           2303.268299 0.000000e+00   2
# Diesel.US.NY.Daily -> GDELT           2041.951076 0.000000e+00   3
# GDELT -> Diesel.US.NY.Daily              3.786173 9.969728e-03   3
# Diesel.US.NY.Daily -> GDELT           1920.386665 0.000000e+00   4
# GDELT -> Diesel.US.NY.Daily              4.214410 2.073944e-03   4
# Diesel.US.NY.Daily -> GDELT           2020.677590 0.000000e+00   5
# GDELT -> Diesel.US.NY.Daily              5.160105 9.910062e-05   5
# Diesel.US.NY.Daily -> GDELT           2180.264062 0.000000e+00   6
# GDELT -> Diesel.US.NY.Daily              5.867601 4.049526e-06   6
# Diesel.US.NY.Daily -> GDELT           2141.737016 0.000000e+00   7
# GDELT -> Diesel.US.NY.Daily              4.994321 1.169336e-05   7
# Diesel.US.NY.Daily -> GDELT           2124.694976 0.000000e+00   8
# GDELT -> Diesel.US.NY.Daily              5.111519 2.265934e-06   8
# Diesel.US.NY.Daily -> GDELT           2102.521852 0.000000e+00   9
# GDELT -> Diesel.US.NY.Daily              5.467863 1.599543e-07   9
# Diesel.US.NY.Daily -> GDELT           2158.474302 0.000000e+00  10
# GDELT -> Diesel.US.NY.Daily              3.801117 3.883811e-05  10
# Diesel.US.Gulf.Coast.Daily -> GDELT   3780.820940 0.000000e+00   1
# GDELT -> Diesel.US.Gulf.Coast.Daily      7.051709 7.934104e-03   1
# Diesel.US.Gulf.Coast.Daily -> GDELT   2301.469103 0.000000e+00   2
# Diesel.US.Gulf.Coast.Daily -> GDELT   2043.159451 0.000000e+00   3
# Diesel.US.Gulf.Coast.Daily -> GDELT   1924.207095 0.000000e+00   4
# GDELT -> Diesel.US.Gulf.Coast.Daily      5.636174 1.581153e-04   4
# Diesel.US.Gulf.Coast.Daily -> GDELT   2021.586963 0.000000e+00   5
# GDELT -> Diesel.US.Gulf.Coast.Daily      4.888627 1.809339e-04   5
# Diesel.US.Gulf.Coast.Daily -> GDELT   2180.199724 0.000000e+00   6
# GDELT -> Diesel.US.Gulf.Coast.Daily      4.515624 1.413484e-04   6
# Diesel.US.Gulf.Coast.Daily -> GDELT   2137.969920 0.000000e+00   7
# GDELT -> Diesel.US.Gulf.Coast.Daily      4.788446 2.171671e-05   7
# Diesel.US.Gulf.Coast.Daily -> GDELT   2120.181900 0.000000e+00   8
# GDELT -> Diesel.US.Gulf.Coast.Daily      3.889281 1.366895e-04   8
# Diesel.US.Gulf.Coast.Daily -> GDELT   2100.103017 0.000000e+00   9
# GDELT -> Diesel.US.Gulf.Coast.Daily      3.714007 1.149906e-04   9
# Diesel.US.Gulf.Coast.Daily -> GDELT   2156.087650 0.000000e+00  10
# GDELT -> Diesel.US.Gulf.Coast.Daily      2.929078 1.133694e-03  10
# Diesel.US.LA.Daily -> GDELT           1176.449603 0.000000e+00   1
# Diesel.US.LA.Daily -> GDELT            980.035167 0.000000e+00   2
# Diesel.US.LA.Daily -> GDELT           1014.062037 0.000000e+00   3
# Diesel.US.LA.Daily -> GDELT           1028.393468 0.000000e+00   4
# Diesel.US.LA.Daily -> GDELT           1136.281970 0.000000e+00   5
# Diesel.US.LA.Daily -> GDELT           1283.082776 0.000000e+00   6
# Diesel.US.LA.Daily -> GDELT           1256.674168 0.000000e+00   7
# Diesel.US.LA.Daily -> GDELT           1255.308278 0.000000e+00   8
# Diesel.US.LA.Daily -> GDELT           1232.091065 0.000000e+00   9
# Diesel.US.LA.Daily -> GDELT           1252.347730 0.000000e+00  10
# Gasoline.US.NY.Daily -> GDELT          916.661905 0.000000e+00   1
# Gasoline.US.NY.Daily -> GDELT          895.312857 0.000000e+00   2
# Gasoline.US.NY.Daily -> GDELT          971.438974 0.000000e+00   3
# Gasoline.US.NY.Daily -> GDELT         1000.434227 0.000000e+00   4
# Gasoline.US.NY.Daily -> GDELT         1118.228131 0.000000e+00   5
# Gasoline.US.NY.Daily -> GDELT         1267.248229 0.000000e+00   6
# Gasoline.US.NY.Daily -> GDELT         1245.016032 0.000000e+00   7
# Gasoline.US.NY.Daily -> GDELT         1246.150191 0.000000e+00   8
# Gasoline.US.NY.Daily -> GDELT         1225.916493 0.000000e+00   9
# Gasoline.US.NY.Daily -> GDELT         1249.168474 0.000000e+00  10
# Gasoline.US.Gulf.Coast.Daily -> GDELT  916.541287 0.000000e+00   1
# Gasoline.US.Gulf.Coast.Daily -> GDELT  894.758228 0.000000e+00   2
# Gasoline.US.Gulf.Coast.Daily -> GDELT  971.567538 0.000000e+00   3
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1001.480740 0.000000e+00   4
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1117.061416 0.000000e+00   5
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1265.717313 0.000000e+00   6
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1243.140051 0.000000e+00   7
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1243.875988 0.000000e+00   8
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1223.430776 0.000000e+00   9
# Gasoline.US.Gulf.Coast.Daily -> GDELT 1246.060394 0.000000e+00  10
# Gasoline.US.LA.Daily -> GDELT         2350.840879 0.000000e+00   1
# Gasoline.US.LA.Daily -> GDELT         1585.957901 0.000000e+00   2
# Gasoline.US.LA.Daily -> GDELT         1478.360517 0.000000e+00   3
# Gasoline.US.LA.Daily -> GDELT         1418.902386 0.000000e+00   4
# GDELT -> Gasoline.US.LA.Daily            4.912726 5.907797e-04   4
# Gasoline.US.LA.Daily -> GDELT         1523.986267 0.000000e+00   5
# Gasoline.US.LA.Daily -> GDELT         1672.665781 0.000000e+00   6
# GDELT -> Gasoline.US.LA.Daily            5.442355 1.252965e-05   6
# Gasoline.US.LA.Daily -> GDELT         1635.300168 0.000000e+00   7
# GDELT -> Gasoline.US.LA.Daily            4.759738 2.366741e-05   7
# Gasoline.US.LA.Daily -> GDELT         1627.476131 0.000000e+00   8
# GDELT -> Gasoline.US.LA.Daily            5.030601 2.988881e-06   8
# Gasoline.US.LA.Daily -> GDELT         1601.359041 0.000000e+00   9
# GDELT -> Gasoline.US.LA.Daily            6.618290 1.767857e-09   9
# Gasoline.US.LA.Daily -> GDELT         1626.421732 0.000000e+00  10
# GDELT -> Gasoline.US.LA.Daily            9.127319 3.663736e-15  10
# Heating.Oil.US.NY.Daily -> GDELT       922.119445 0.000000e+00   1
# Heating.Oil.US.NY.Daily -> GDELT       899.017072 0.000000e+00   2
# Heating.Oil.US.NY.Daily -> GDELT       976.983202 0.000000e+00   3
# Heating.Oil.US.NY.Daily -> GDELT      1005.176316 0.000000e+00   4
# GDELT -> Heating.Oil.US.NY.Daily         3.698787 5.171437e-03   4
# Heating.Oil.US.NY.Daily -> GDELT      1120.556241 0.000000e+00   5
# GDELT -> Heating.Oil.US.NY.Daily         3.484883 3.785591e-03   5
# Heating.Oil.US.NY.Daily -> GDELT      1269.638884 0.000000e+00   6
# GDELT -> Heating.Oil.US.NY.Daily         3.122227 4.669791e-03   6
# Heating.Oil.US.NY.Daily -> GDELT      1246.847656 0.000000e+00   7
# GDELT -> Heating.Oil.US.NY.Daily         3.036037 3.442304e-03   7
# Heating.Oil.US.NY.Daily -> GDELT      1247.376809 0.000000e+00   8
# GDELT -> Heating.Oil.US.NY.Daily         2.969413 2.543305e-03   8
# Heating.Oil.US.NY.Daily -> GDELT      1225.356051 0.000000e+00   9
# GDELT -> Heating.Oil.US.NY.Daily         2.705228 3.820845e-03   9
# Heating.Oil.US.NY.Daily -> GDELT      1248.342755 0.000000e+00  10
# Henry.Hub.Natural.Gas.Daily -> GDELT  1215.504988 0.000000e+00   1
# Henry.Hub.Natural.Gas.Daily -> GDELT   994.586821 0.000000e+00   2
# Henry.Hub.Natural.Gas.Daily -> GDELT  1020.903570 0.000000e+00   3
# Henry.Hub.Natural.Gas.Daily -> GDELT  1033.433173 0.000000e+00   4
# Henry.Hub.Natural.Gas.Daily -> GDELT  1140.568979 0.000000e+00   5
# Henry.Hub.Natural.Gas.Daily -> GDELT  1289.372079 0.000000e+00   6
# Henry.Hub.Natural.Gas.Daily -> GDELT  1261.278417 0.000000e+00   7
# Henry.Hub.Natural.Gas.Daily -> GDELT  1258.280955 0.000000e+00   8
# Henry.Hub.Natural.Gas.Daily -> GDELT  1235.495649 0.000000e+00   9
# Henry.Hub.Natural.Gas.Daily -> GDELT  1255.741410 0.000000e+00  10
# Kerosene.Jet.Fuel.Daily -> GDELT       990.541364 0.000000e+00   1
# Kerosene.Jet.Fuel.Daily -> GDELT       919.954251 0.000000e+00   2
# Kerosene.Jet.Fuel.Daily -> GDELT       984.604805 0.000000e+00   3
# Kerosene.Jet.Fuel.Daily -> GDELT      1009.333105 0.000000e+00   4
# Kerosene.Jet.Fuel.Daily -> GDELT      1122.816247 0.000000e+00   5
# Kerosene.Jet.Fuel.Daily -> GDELT      1270.428926 0.000000e+00   6
# Kerosene.Jet.Fuel.Daily -> GDELT      1247.513472 0.000000e+00   7
# Kerosene.Jet.Fuel.Daily -> GDELT      1247.028475 0.000000e+00   8
# Kerosene.Jet.Fuel.Daily -> GDELT      1224.635762 0.000000e+00   9
# Kerosene.Jet.Fuel.Daily -> GDELT      1245.628449 0.000000e+00  10
# Oil.Brent.Daily -> GDELT               934.381486 0.000000e+00   1
# Oil.Brent.Daily -> GDELT               902.408338 0.000000e+00   2
# Oil.Brent.Daily -> GDELT               976.345090 0.000000e+00   3
# Oil.Brent.Daily -> GDELT              1011.441037 0.000000e+00   4
# GDELT -> Oil.Brent.Daily                 3.980709 3.143075e-03   4
# Oil.Brent.Daily -> GDELT              1129.308290 0.000000e+00   5
# GDELT -> Oil.Brent.Daily                 5.672499 3.152532e-05   5
# Oil.Brent.Daily -> GDELT              1279.514874 0.000000e+00   6
# GDELT -> Oil.Brent.Daily                 4.449124 1.677862e-04   6
# Oil.Brent.Daily -> GDELT              1251.401105 0.000000e+00   7
# GDELT -> Oil.Brent.Daily                 4.364827 7.664098e-05   7
# Oil.Brent.Daily -> GDELT              1250.868291 0.000000e+00   8
# GDELT -> Oil.Brent.Daily                 3.738186 2.235499e-04   8
# Oil.Brent.Daily -> GDELT              1228.917666 0.000000e+00   9
# GDELT -> Oil.Brent.Daily                 3.473406 2.719194e-04   9
# Oil.Brent.Daily -> GDELT              1253.155647 0.000000e+00  10
# GDELT -> Oil.Brent.Daily                 3.616316 8.085325e-05  10
# Oil.Canada.Monthly -> GDELT           7785.064256 0.000000e+00   1
# GDELT -> Oil.Canada.Monthly             32.671243 1.129181e-08   1
# Oil.Canada.Monthly -> GDELT           4254.806908 0.000000e+00   2
# GDELT -> Oil.Canada.Monthly             67.993128 0.000000e+00   2
# Oil.Canada.Monthly -> GDELT           3464.361227 0.000000e+00   3
# GDELT -> Oil.Canada.Monthly             32.696830 0.000000e+00   3
# Oil.Canada.Monthly -> GDELT           3166.392208 0.000000e+00   4
# GDELT -> Oil.Canada.Monthly             24.410433 0.000000e+00   4
# Oil.Canada.Monthly -> GDELT           3252.319855 0.000000e+00   5
# GDELT -> Oil.Canada.Monthly             26.278741 0.000000e+00   5
# Oil.Canada.Monthly -> GDELT           3503.621647 0.000000e+00   6
# GDELT -> Oil.Canada.Monthly              7.473355 5.295581e-08   6
# Oil.Canada.Monthly -> GDELT           3446.066619 0.000000e+00   7
# GDELT -> Oil.Canada.Monthly             10.033307 1.506573e-12   7
# Oil.Canada.Monthly -> GDELT           3422.421191 0.000000e+00   8
# GDELT -> Oil.Canada.Monthly              8.956858 2.618572e-12   8
# Oil.Canada.Monthly -> GDELT           3438.852185 0.000000e+00   9
# GDELT -> Oil.Canada.Monthly              9.442192 1.976197e-14   9
# Oil.Canada.Monthly -> GDELT           3626.878075 0.000000e+00  10
# GDELT -> Oil.Canada.Monthly             14.511181 0.000000e+00  10
# Oil.Fateh.Monthly -> GDELT             925.548701 0.000000e+00   1
# Oil.Fateh.Monthly -> GDELT             905.518877 0.000000e+00   2
# Oil.Fateh.Monthly -> GDELT             979.526755 0.000000e+00   3
# Oil.Fateh.Monthly -> GDELT            1008.592527 0.000000e+00   4
# Oil.Fateh.Monthly -> GDELT            1123.211283 0.000000e+00   5
# Oil.Fateh.Monthly -> GDELT            1266.793676 0.000000e+00   6
# Oil.Fateh.Monthly -> GDELT            1240.642028 0.000000e+00   7
# Oil.Fateh.Monthly -> GDELT            1241.778619 0.000000e+00   8
# Oil.Fateh.Monthly -> GDELT            1221.027462 0.000000e+00   9
# Oil.Fateh.Monthly -> GDELT            1242.452283 0.000000e+00  10
# Oil.OPEC.Daily -> GDELT               2290.202851 0.000000e+00   1
# Oil.OPEC.Daily -> GDELT               1551.258671 0.000000e+00   2
# Oil.OPEC.Daily -> GDELT               1449.334663 0.000000e+00   3
# GDELT -> Oil.OPEC.Daily                  6.195673 3.359006e-04   3
# Oil.OPEC.Daily -> GDELT               1398.139048 0.000000e+00   4
# GDELT -> Oil.OPEC.Daily                  6.349328 4.255251e-05   4
# Oil.OPEC.Daily -> GDELT               1500.629901 0.000000e+00   5
# GDELT -> Oil.OPEC.Daily                  8.075951 1.305834e-07   5
# Oil.OPEC.Daily -> GDELT               1649.467256 0.000000e+00   6
# GDELT -> Oil.OPEC.Daily                  7.674163 3.059198e-08   6
# Oil.OPEC.Daily -> GDELT               1608.299517 0.000000e+00   7
# GDELT -> Oil.OPEC.Daily                  6.606991 8.232046e-08   7
# Oil.OPEC.Daily -> GDELT               1600.119000 0.000000e+00   8
# GDELT -> Oil.OPEC.Daily                  8.644073 8.171019e-12   8
# Oil.OPEC.Daily -> GDELT               1571.139578 0.000000e+00   9
# GDELT -> Oil.OPEC.Daily                  7.613872 3.319833e-11   9
# Oil.OPEC.Daily -> GDELT               1596.257950 0.000000e+00  10
# GDELT -> Oil.OPEC.Daily                  6.414434 6.497025e-10  10
# Oil.WTI.Daily -> GDELT                 913.824741 0.000000e+00   1
# Oil.WTI.Daily -> GDELT                 897.055436 0.000000e+00   2
# Oil.WTI.Daily -> GDELT                 973.466993 0.000000e+00   3
# Oil.WTI.Daily -> GDELT                1002.984961 0.000000e+00   4
# GDELT -> Oil.WTI.Daily                   3.614505 5.996526e-03   4
# Oil.WTI.Daily -> GDELT                1120.019076 0.000000e+00   5
# GDELT -> Oil.WTI.Daily                   3.620767 2.838900e-03   5
# Oil.WTI.Daily -> GDELT                1269.783498 0.000000e+00   6
# Oil.WTI.Daily -> GDELT                1245.388449 0.000000e+00   7
# GDELT -> Oil.WTI.Daily                   3.795277 4.044172e-04   7
# Oil.WTI.Daily -> GDELT                1246.532350 0.000000e+00   8
# GDELT -> Oil.WTI.Daily                   3.752568 2.133604e-04   8
# Oil.WTI.Daily -> GDELT                1225.735467 0.000000e+00   9
# GDELT -> Oil.WTI.Daily                   2.715137 3.697360e-03   9
# Oil.WTI.Daily -> GDELT                1250.199311 0.000000e+00  10
# GDELT -> Oil.WTI.Daily                   2.331181 9.724409e-03  10
# Propane.Mont.Belvieu.Daily -> GDELT   1048.244506 0.000000e+00   1
# Propane.Mont.Belvieu.Daily -> GDELT    939.708953 0.000000e+00   2
# Propane.Mont.Belvieu.Daily -> GDELT    996.149973 0.000000e+00   3
# Propane.Mont.Belvieu.Daily -> GDELT   1016.905465 0.000000e+00   4
# GDELT -> Propane.Mont.Belvieu.Daily      4.698100 8.707400e-04   4
# Propane.Mont.Belvieu.Daily -> GDELT   1131.287743 0.000000e+00   5
# GDELT -> Propane.Mont.Belvieu.Daily      3.898169 1.569670e-03   5
# Propane.Mont.Belvieu.Daily -> GDELT   1278.640124 0.000000e+00   6
# Propane.Mont.Belvieu.Daily -> GDELT   1251.951001 0.000000e+00   7
# GDELT -> Propane.Mont.Belvieu.Daily      2.870540 5.408914e-03   7
# Propane.Mont.Belvieu.Daily -> GDELT   1251.922021 0.000000e+00   8
# GDELT -> Propane.Mont.Belvieu.Daily      3.138494 1.507343e-03   8
# Propane.Mont.Belvieu.Daily -> GDELT   1231.320501 0.000000e+00   9
# GDELT -> Propane.Mont.Belvieu.Daily      2.508586 7.274219e-03   9
# Propane.Mont.Belvieu.Daily -> GDELT   1255.081205 0.000000e+00  10
# GDELT -> Propane.Mont.Belvieu.Daily      2.569010 4.221364e-03  10
# ##########################################################################