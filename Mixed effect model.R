# CA683 - Assignment 2 - Mixed effect model
#

# Packages Used
# install.packages("xts")
# install.packages("forecast")
# install.packages("lmtest")
# install.packages("vars")
# install.packages("MSBVAR")
# install.packages("MASS")
# install.packages("Matrix")
# install.packages("lme4")
require("xts")
require("forecast")
require("lmtest")
require("vars")
require("MSBVAR")
require("MASS")
require("Matrix")
require("lme4")

##load the data
# GDELT Event Counts
gdelt <- readRDS("gdelt_indicators.rds")
# Oil and Derivatives
oil_and_derivates <- readRDS("oil_and_derivates.rds")
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

##my model
head(oil_gdelt)
oil.dta <- na.omit(oil_gdelt)
cor(oil.dta[ , 1:13])

##model selection
m1=glm(EventHappened~.^2,family=binomial,data=oil.dta)
n=dim(oil.dta)[1]
MAIC=stepAIC(m1,direction=('backward'))#AIC
##The least AIC model

##The factors may affect the result EventHappeened
glm1<- glm(EventHappened ~ Oil.Canada.Monthly + 
                    Diesel.US.Gulf.Coast.Daily:Gasoline.US.Gulf.Coast.Daily + 
                    Gasoline.US.NY.Daily:Henry.Hub.Natural.Gas.Daily + 
                    Gasoline.US.Gulf.Coast.Daily:Kerosene.Jet.Fuel.Daily,
                    family="binomial", data=oil.dta)
summary(glm1)
prob=predict(glm1,type=c("response"))
oil.dta$prob=prob
library(pROC)
g <- roc(EventHappened ~ prob, data = oil.dta)
plot(g) 

##Through the model we selected, we are able to pick out the most impactable factors. Acoording to the model
## oil.Canada.Monthly increase one unit, the log odds of event happen will decrease by 0.66,
## when Diesel.US.Gulf.Coast.Daily:Gasoline.US.Gulf.Coast.Daily integrated effects increase 1 unit, the log odds
##of event will decrease by 215.91618. When Gasoline.US.NY.Daily:Henry.Hub.Natural.Gas.Daily increase one unit,
## the log odds of event will decrease by 27.00923. When Gasoline.US.Gulf.Coast.Daily:Kerosene.Jet.Fuel.Daily
##increase one unit, the log odds of event will decrease by 199.52388.

anova(glm1)
## This the the anova table which showed prviously
plot(glm1)

##From the picture, we can see our model fits very well.

##Based on the previous model. we want to see if the other factors are also included in the model.
library(Matrix)
library(lme4)
model.mix <- glmer(oil.dta$EventHappened ~ oil.dta$Oil.Canada.Monthly + (1|oil.dta$Oil.OPEC.Daily),family = binomial)
summary(model.mix) 
##Based on previous results, now we build a generalized logistic regression with random effect. We build
##such a model because we knwo exactly that event are affected by oil price in Canada. However, whem there
## are special event happened in OPEC, it may also affect the results.From the correlation, we can view this as 
## an very important one.





