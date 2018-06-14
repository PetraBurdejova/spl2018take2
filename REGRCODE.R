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
#####shortcut#####
r <- read.csv("r.csv")

#####Assault#####
###model
model_Assault0<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
#plot(x=r$Hood_ID,y=residuals(model_Assault0),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
#sort(model_Assault0$residuals)[c(1,140)]
#plot(dffits(model_Assault0))
#dffits(model_Assault0)[dffits(model_Assault0)>2*sqrt(14/140)]
#plot(rstudent(model_Assault0))
plot(cooks.distance(model_Assault0))
sort(cooks.distance(model_Assault0))
cooks.distance(model_Assault0)[cooks.distance(model_Assault0)>4/140]
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
###model
model_Auto.Theft0<-lm(Auto.Theft~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_Auto.Theft0))
cooks.distance(model_Auto.Theft0)[cooks.distance(model_Auto.Theft0)>4/140]
sort(cooks.distance(model_Auto.Theft0))
r_Auto.Theft <- r[-c(1,21,41,76,109,110),]
model_Auto.Theft<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Auto.Theft)
summary(model_Auto.Theft)

###variable selection using backward aic
step(model_Auto.Theft)

###normality
ad.test(residuals(model_Auto.Theft)) 
shapiro.test(residuals(model_Auto.Theft))
lillie.test(residuals(model_Auto.Theft))

###homoscedasticity
bptest(model_)

#####Break.and.Enter#####
###model
model_Break.and.Enter0<-lm(Break.and.Enter~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_Break.and.Enter0))
cooks.distance(model_Break.and.Enter0)[cooks.distance(model_Break.and.Enter0)>4/140]
sort(cooks.distance(model_))
r_Break.and.Enter <- r[-c(31, 41, 66, 71, 73, 76, 79, 121),]
model_Break.and.Enter<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Break.and.Enter)
summary(model_Break.and.Enter)

###variable selection using backward aic
step(model_Break.and.Enter)

###normality
ad.test(residuals(model_Break.and.Enter)) 
shapiro.test(residuals(model_Break.and.Enter)) 
lillie.test(residuals(model_Break.and.Enter))

###homoscedasticity
bptest(model_Break.and.Enter)

#####Robbery#####
###model
model_Robbery0<-lm(Robbery~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_Robbery0))
cooks.distance(model_Robbery0)[cooks.distance(model_Robbery0)>4/140]
sort(cooks.distance(model_Robbery0))
r_Robbery <- r[-c(66, 73, 75, 76, 79, 112),]
model_Robbery<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Robbery)
summary(model_Robbery)

###variable selection using backward aic
step(model_Robbery)

###normality
ad.test(residuals(model_Robbery)) 
shapiro.test(residuals(model_Robbery)) 
lillie.test(residuals(model_Robbery))

###homoscedasticity
bptest(model_Robbery)

#####Theft.Over#####
###model
model_Theft.Over0<-lm(Theft.Over~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_Theft.Over0))
cooks.distance(model_Theft.Over0)[cooks.distance(model_Theft.Over0)>4/140]
sort(cooks.distance(model_Theft.Over0))
r_Theft.Over <- r[-c(1,21,27,31,55,75,76,121),]
model_Theft.Over<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Theft.Over)
summary(model_Theft.Over)

###variable selection using backward aic
step(model_Theft.Over)

###normality
ad.test(residuals(model_Theft.Over)) 
shapiro.test(residuals(model_Theft.Over))
lillie.test(residuals(model_Theft.Over))

###homoscedasticity
bptest(model_Theft.Over)

#####Drug.Arrests#####
###model
model_Drug.Arrests0<-lm(Drug.Arrests~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_Drug.Arrests0))
cooks.distance(model_Drug.Arrests0)[cooks.distance(model_Drug.Arrests0)>4/140]
sort(cooks.distance(model_Drug.Arrests0))
r_Drug.Arrests <- r[-c(72,73,75,76,78,79),]
model_Drug.Arrests<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Drug.Arrests)
summary(model_Drug.Arrests)

###variable selection using backward aic
step(model_Drug.Arrests)

###normality
ad.test(residuals(model_Drug.Arrests)) 
shapiro.test(residuals(model_Drug.Arrests)) 
lillie.test(residuals(model_Drug.Arrests))

###homoscedasticity
bptest(model_Drug.Arrests)

#####Total.crime#####
###model
model_Total.crime0<-lm(Total.crime~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_Total.crime0))
cooks.distance(model_Total.crime0)[cooks.distance(model_Total.crime0)>4/140]
sort(cooks.distance(model_Total.crime0))
r_Total.crime <- r[-c(21, 73, 75, 76, 78, 79, 98, 121),]
model_Total.crime<-lm(Assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_Total.crime)
summary(model_Total.crime)

###variable selection using backward aic
step(model_Total.crime)

###normality
ad.test(residuals(model_Total.crime)) 
shapiro.test(residuals(model_Total.crime)) 
lillie.test(residuals(model_Total.crime))

###homoscedasticity
bptest(model_Total.crime)