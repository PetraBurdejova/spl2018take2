#read csv
regressiondata <- read.csv("regressiondata.csv")

#regression
model<-lm(Robbery~Unemployed,data=agg)
summary(model)

#plot errors
plot(x=agg$Hood_ID,y=residuals(model),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))

#autocorrelation
library(car)
dwt(model)

#normailty
library(nortest)
ad.test(residuals(model)) 
shapiro.test(residuals(model)) 
lillie.test(residuals(model))