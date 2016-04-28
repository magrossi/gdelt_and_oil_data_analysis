require("tseries")
require("ForeCA")
# Pre-process: PCA requirethe number of rows of each column the same. So i remove some of the columns 
# , which have less data than others. And rows begin from 1992-07-09
oil_and_derivates <- readRDS("./data/oil_and_derivates.rds")
newdata <- oil_and_derivates[,c(4, 5, 7, 9, 10, 12, 14, 15)]
newdata <- newdata[c(1767:7970)]

# PCA: The first three components are most predicable
ff <- foreca(newdata, n.comp = 4, plot = TRUE)
plot(ff)
summary(ff)
pc_oil_price <- data.frame(matrix(unlist(ff["scores"]), nrow=6204, byrow=T))
saveRDS(pc_oil_price, "./data/pc_oil_price.rds.")