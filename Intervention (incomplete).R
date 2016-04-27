#CA683 Assignment 2 - Intervention Analysis

#Packages
#install.packages("foreign")
#install.packages("TSA")
#install.packages("xts")
library(foreign)
library(TSA)
library(xts)

#For Mac, set the path of data file.
setwd('/Users/xuanyang/Documents/CA683 Data Analytics and Data Mining/ca683_project/data')

#####################Part 1 Data Preparation##############
#Download the processed datasets at: 
#https://github.com/magrossi/ca683_project/tree/master/data
##########################################################

#Get one of the oil daily data from 2000 to 2015.
oil_wti_daily <- readRDS("oil_wti_daily.rds")
oil_wti_00 <- oil_wti_daily["2000-01-01::2015-12-31"]
#Get the gdelt data with indicators from 2000 to 2015.
gdelt_indicators <- readRDS("gdelt_indicators.rds")
gdelt_indicators_00 <- gdelt_indicators["2000-01-01::2015-12-31"]
#Get the amount of event in each day.
gdelt_00 <- gdelt_indicators_00[,c("EventCount")]
#For this analysis, test_1 created
test_1 <- oil_wti_00
#Make the same name "Event" to the gdelt dataset.
names(gdelt_00) <- c("Event")
#Merge oil dataset and gdelt dataset together.
test_1 <- merge(oil_wti_00, gdelt_00)

#1 means event happened, 0 means didn't.
#test_1$Event[is.na(test_1$Event)] <- 0
#test_1$Event[test_1$Event != 0] <- 1

#Here some of the oil data is missing, I use its mean to replace it.
#test_1$Oil.WTI.Daily[is.na(test_1$Oil.WTI.Daily)] <- mean(test_1$Oil.WTI.Daily, na.rm = TRUE)
#Here you can have a quick visible comparsion between the oil price and the times of event.
plot.zoo(test_1)

###########################################################
#From the chat test_1 above, the huge change of the oil price 
#seems happened around 2008, while the events (GDELT code 20)
#most happened after 2008. In this case, there has a doubt of 
#whether the oil price has a connection with the event times.
######################End of Part 1#########################


#################Part 2 Analysis (Simple)####################
#Refer to: 
#http://spia.uga.edu/faculty_pages/monogan/teaching/ts/Dinterv.pdf
############################################################

#Get time series of oil price
oil_ts = ts(test_1$Oil.WTI.Daily, start = 2000, frequency = 300)
plot.ts(oil_ts)

#From Part 1's result, the huge change or oil price happened around 2008,
#so set 2008 as the time classifier of the intervention indicator (0 or 1),
#and create lists for each indicator.
ind=rep(0,length(oil_ts[time(oil_ts)<2008])) 
length(ind) 
ind1=rep(1,length(oil_ts[time(oil_ts)>=2008])) 
length(ind1)
#Create a new set contains indicators.
indd=c(ind,ind1)

#Fit the intervention model
fit=arimax(oil_ts,order=c(1,1,0),xtransf=indd,transfer=list(c(1,0)))
plot.ts(oil_ts)
plot(oil_ts,lty=2)
lines(ts(fitted(fit), start = 2000, frequency = 300)) 
legend("topleft",c("Observed","Fitted"),lty=c(2,1))

################Unexpected result#############

#Method 2
mod.2 <- arimax(test_1$Oil.WTI.Daily, order=c(1,0,0), xtransf=indd, transfer=list(c(1,0)))
mod.2
#Result:
#Coefficients:
#  ar1  intercept  T1-AR1  T1-MA0
#0.9988    63.7473  0.7517  0.9132
#s.e.  0.0007    16.3103  0.3398  1.1270
y.diff <- diff(test_1$Oil.WTI.Daily)
y.pred <- 0.9132*indd + 0.9132*(0.7517^(time(oil_ts)<2008))*as.numeric(time(oil_ts)>=2013)
plot(y=y.diff, x=time(oil_ts),type='l')
y.pred <- y.pred[-1]
lines(y=y.pred, x=time(oil_ts)[-1], lty=2)






