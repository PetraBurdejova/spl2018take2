#####read csv#####
regressiondata <- read.csv("regressiondata.csv")
r <- regressiondata
#dependent variables (don´t run it)
r$Assault
r$Auto.Theft
r$Break.and.Enter
r$Robbery
r$Theft.Over
r$Drug.Arrests
r$Total.crime
#independent variables (don´t run it)
r$lone.parent.families.perc
r$avg.income
r$people.ei.per
r$median.income
r$low.income.pop.perc
r$low.income.pop.perc.18.to.64
r$non.citizens.perc
r$immigrants.perc
r$immigrants.recent.perc
r$refugees.perc
r$vis.minorities.perc
r$renters.perc
r$houses.perc
r$unsuitable.housing.perc
r$hhlds.mjr.rprs.perc
r$unaffordable.housing.perc
r$less.than.high.school.perc
r$high.school.cert.perc
r$post.sec.or.above.perc
r$unemployment.rate
r$youth.perc
r$male.youth.perc
r$low.income.pop.perc
r$middle.income.perc
r$high.income.perc
#lone.parent.families.perc + avg.income + people.ei.per + median.income + low.income.pop.perc + low.income.pop.perc.18.to.64 + non.citizens.perc + immigrants.perc + immigrants.recent.perc + refugees.perc + vis.minorities.perc + renters.perc + houses.perc + unsuitable.housing.perc + hhlds.mjr.rprs.perc + unaffordable.housing.perc + less.than.high.school.perc + high.school.cert.perc + post.sec.or.above.perc + unemployment.rate + youth.perc + male.youth.perc + low.income.pop.perc + middle.income.perc + high.income.perc

#preselection of regressors to avoid multicollinearity
library(PerformanceAnalytics) #for chart.Correlation
chart.Correlation(r[,10:35], histogram=TRUE)
cortable <- cor(r[,10:35])
cortablefiltered <- ifelse(cortable>0.5, cortable, NA)

r$avg.income <- NULL #high correlation with median.income, median.income is a more robust representation
r$low.income.pop.perc.18.to.64 <- NULL #high correlation with low.income pop.perc
r$non.citizens.perc <- NULL; r$immigrants.recent.perc <- NULL; r$refugees.perc <- NULL; r$vis.minorities.perc <- NULL #as high correlation with variable r$immigrants.perc
r$unaffordable.housing.perc <- NULL; r$unsuitable.housing.perc <- NULL #high correlation with r$low.income.pop.perc
r$high.school.cert.perc <- NULL; r$post.sec.or.above.perc <- NULL #high correlation with income variables
r$unemployment.rate <- NULL #high correlation with unemployment.rate.males
r$youth.perc <- NULL #high correlation with male.youth; studies show male youth the highest risk of crime activity
r$low.income.perc <- NULL; r$middle.income.perc <- NULL; r$high.income.perc <- NULL #high correlation with income variables
r$lone.parent.families.perc <- NULL #high correlation with several variables
r$people.ei.per <- NULL ; r$low.income.pop.perc <- NULL #high correlaiton with income variables
r$hhlds.mjr.rprs.perc <- NULL #high correlatio with r$renters
r$renters.perc <- NULL

cortable <- cor(r[,10:15])
cortablefiltered <- ifelse(cortable>0.5, cortable, NA)
chart.Correlation(r[,10:15], histogram=TRUE)

#median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc

#####regression models#####
r <- read.csv("r.csv")

model_Assault0<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

model_Auto.Theft0<-lm(Auto.Theft~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

model_Break.and.Enter0<-lm(Break.and.Enter~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

model_Robbery0<-lm(Robbery~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

model_Theft.Over0<-lm(Theft.Over~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

model_Drug.Arrests0<-lm(Drug.Arrests~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

model_Total.crime<-lm(Total.crime~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

#####Assault#####
###outliers and influential observations
#plot(x=r$Hood_ID,y=residuals(model_Assault0),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
#sort(model_Assault0$residuals)[c(1,140)]
#cooks.distance(model_Assault0)[cooks.distance(model_Assault0)>4/140]
#plot(dffits(model_Assault0))
#dffits(model_Assault0)[dffits(model_Assault0)>2*sqrt(14/140)]
#plot(rstudent(model_Assault0))
plot(cooks.distance(model_Assault0))
sort(cooks.distance(model_Assault0))
r_Assault <- r[-c(51, 73, 75, 76, 77, 78, 79, 98),]
model_Assault<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Assault)
summary(model_Assault)

###variable selection
#backward selection using aic
step(model_Assault)
#backward selection using bic
step(model_Assault, k=log(140))
#backward selection using significance of coefficients

#check correlations
#variance inflation factors

###normality
library(nortest)
ad.test(residuals(model_Assault)) 
shapiro.test(residuals(model_Assault)) 
lillie.test(residuals(model_Assault))

###homoscedasticity
library(lmtest)
bptest(model_Assault)
#library(car);leveneTest(model_Assault)

###nonlinearity
crPlots(model_Assault)

#####Auto.Theft#####
summary(model_Auto.Theft)
plot(x=r$Hood_ID,y=residuals(model_Auto.Theft),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))

#####Break.and.Enter#####
summary(model_Break.and.Enter)
plot(x=r$Hood_ID,y=residuals(model_Break.and.Enter),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))

#####Robbery#####
summary(model_Robbery)
plot(x=r$Hood_ID,y=residuals(model_Robbery),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))

#####Theft.Over#####
summary(model_Theft.Over)
plot(x=r$Hood_ID,y=residuals(model_Theft.Over),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))

#####Drug.Arrests#####

#####Total.crime#####
