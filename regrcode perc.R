#####overview#####
#Merging.R -> regressiondata0.csv -> preparation -> regressiondata/r -> preselection -> r -> regression analysis
#####preparation#####
regressiondata <- agg.2016

regressiondata$assault <- regressiondata$assault/regressiondata$population.2016
regressiondata$auto.theft <- regressiondata$auto.theft/regressiondata$population.2016
regressiondata$break.and.enter <- regressiondata$break.and.enter/regressiondata$population.2016
regressiondata$robbery <- regressiondata$robbery/regressiondata$population.2016
regressiondata$theft.over <- regressiondata$theft.over/regressiondata$population.2016
regressiondata$drug.arrests <- regressiondata$drug.arrests/regressiondata$population.2016
regressiondata$total.crime <- regressiondata$total.crime/regressiondata$population.2016

regressiondata$youth.perc <- (regressiondata$youth/regressiondata$population.2016)*100 
regressiondata$youth <- NULL
regressiondata$male.youth.perc <- (regressiondata$male.youth/regressiondata$population.2016)*100 
regressiondata$male.youth <- NULL
regressiondata$male.above.15 <- (regressiondata$male.above.15/regressiondata$population.2016)*100 
regressiondata$male.above.15 <- NULL
regressiondata$female.above.15 <- (regressiondata$female.above.15/regressiondata$population.2016)*100 
regressiondata$female.above.15 <- NULL
regressiondata$lone.parent.families.perc <- regressiondata$lone.parent.families.perc*100
regressiondata$lone.parent.families <- NULL
regressiondata$low.income.perc <- (regressiondata$low.income/regressiondata$population.2016)*100
regressiondata$low.income <- NULL
regressiondata$middle.income.perc <- (regressiondata$middle.income/regressiondata$population.2016)*100
regressiondata$middle.income <- NULL
regressiondata$high.income.perc <- (regressiondata$high.income/regressiondata$population.2016)*100
regressiondata$high.income <- NULL

regressiondata$people.ei <- NULL
regressiondata$people.ei.per <- regressiondata$people.ei.per*100
regressiondata$no.hholds.bottom.20per <- NULL
regressiondata$hholds.bottom.20per.per <- NULL
regressiondata$low.income.pop <- NULL
regressiondata$low.income.pop.18.to.64 <- NULL
regressiondata$non.citizens <- NULL
regressiondata$non.citizens.perc <- regressiondata$non.citizens.perc*100
regressiondata$immigrants <- NULL
regressiondata$immigrants.perc <- regressiondata$immigrants.perc*100
regressiondata$immigrants.recent <- NULL
regressiondata$immigrants.recent.perc <- regressiondata$immigrants.recent.perc*100
regressiondata$refugees <- NULL
regressiondata$refugees.perc <- regressiondata$refugees.perc*100
regressiondata$vis.minorities <- NULL
regressiondata$vis.minorities.perc <- regressiondata$vis.minorities.perc*100
regressiondata$renters <- NULL
regressiondata$renters.perc <- regressiondata$renters.perc*100
regressiondata$houses <- NULL
regressiondata$houses.perc <- regressiondata$houses.perc*100
regressiondata$unsuitable.housing <- NULL
regressiondata$unsuitable.housing.perc <- regressiondata$unsuitable.housing.perc*100
regressiondata$hhlds.mjr.rprs <- NULL
regressiondata$hhlds.mjr.rprs.perc <- regressiondata$hhlds.mjr.rprs.perc*100
regressiondata$unaffordable.housing <- NULL
regressiondata$unaffordable.housing.perc <- regressiondata$unaffordable.housing.perc*100
regressiondata$less.than.high.school <- NULL
regressiondata$less.than.high.school.perc <- regressiondata$less.than.high.school.perc*100
regressiondata$high.school.cert <- NULL
regressiondata$high.school.cert.perc <- regressiondata$high.school.cert*100
regressiondata$post.sec.or.above <- NULL
regressiondata$post.sec.or.above.perc <- regressiondata$post.sec.or.above.perc*100
regressiondata$unemployed <- NULL
regressiondata$unemployed.males <- NULL
#regressiondata <- round(regressiondata, digits = 3)
#write.csv(regressiondata, "regressiondata.csv", row.names=FALSE)
#regressiondata <- read.csv("regressiondata.csv")
r <- regressiondata
#dependent variables (don´t run it)
#r$assault
#r$auto.theft
#r$break.and.enter
#r$robbery
#r$theft.over
#r$drug.arrests
#r$total.crime
#independent variables (don´t run it)
#r$lone.parent.families.perc
#r$avg.income
#r$people.ei.per
#r$median.income
#r$low.income.pop.perc
#r$low.income.pop.perc.18.to.64
#r$non.citizens.perc
#r$immigrants.perc
#r$immigrants.recent.perc
#r$refugees.perc
#r$vis.minorities.perc
#r$renters.perc
#r$houses.perc
#r$unsuitable.housing.perc
#r$hhlds.mjr.rprs.perc
#r$unaffordable.housing.perc
#r$less.than.high.school.perc
#r$high.school.cert.perc
#r$post.sec.or.above.perc
#r$unemployment.rate
#r$youth.perc
#r$male.youth.perc
#r$low.income.pop.perc
#r$middle.income.perc
#r$high.income.perc
#lone.parent.families.perc + avg.income + people.ei.per + median.income + low.income.pop.perc + low.income.pop.perc.18.to.64 + non.citizens.perc + immigrants.perc + immigrants.recent.perc + refugees.perc + vis.minorities.perc + renters.perc + houses.perc + unsuitable.housing.perc + hhlds.mjr.rprs.perc + unaffordable.housing.perc + less.than.high.school.perc + high.school.cert.perc + post.sec.or.above.perc + unemployment.rate + youth.perc + male.youth.perc + low.income.pop.perc + middle.income.perc + high.income.perc

#####preselection of regressors#####
#to avoid multicollinearity
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

#r <- read.csv("r.csv")

#####assault#####
###model
model_assault0<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
#plot(x=r$Hood_ID,y=residuals(model_assault0),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
#sort(model_assault0$residuals)[c(1,140)]
#plot(dffits(model_assault0))
#dffits(model_assault0)[dffits(model_assault0)>2*sqrt(14/140)]
#plot(rstudent(model_assault0))
plot(cooks.distance(model_assault0))
sort(cooks.distance(model_assault0))
cooks.distance(model_assault0)[cooks.distance(model_assault0)>4/140]
r_assault <- r[-c(51, 73, 75, 76, 77, 78, 79, 98),]
model_assault<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_assault)
summary(model_assault)

###variable selection
#backward selection using aic
step(model_assault)
#backward selection using bic
step(model_assault, k=log(140))
#backward selection using significance of coefficients

#check correlations
#variance inflation factors

###normality
library(nortest)
ad.test(residuals(model_assault)) 
shapiro.test(residuals(model_assault)) 
lillie.test(residuals(model_assault))

###homoscedasticity
library(lmtest)
bptest(model_assault)
#library(car);leveneTest(model_assault)

###nonlinearity
crPlots(model_assault)

#####auto.theft#####
###model
model_auto.theft0<-lm(auto.theft~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_auto.theft0))
cooks.distance(model_auto.theft0)[cooks.distance(model_auto.theft0)>4/140]
sort(cooks.distance(model_auto.theft0))
r_auto.theft <- r[-c(1,21,41,76,109,110),]
model_auto.theft<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_auto.theft)
summary(model_auto.theft)

###variable selection using backward aic
step(model_auto.theft)

###normality
ad.test(residuals(model_auto.theft)) 
shapiro.test(residuals(model_auto.theft))
lillie.test(residuals(model_auto.theft))

###homoscedasticity
bptest(model_)

#####break.and.enter#####
###model
model_break.and.enter0<-lm(break.and.enter~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_break.and.enter0))
cooks.distance(model_break.and.enter0)[cooks.distance(model_break.and.enter0)>4/140]
sort(cooks.distance(model_))
r_break.and.enter <- r[-c(31, 41, 66, 71, 73, 76, 79, 121),]
model_break.and.enter<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_break.and.enter)
summary(model_break.and.enter)

###variable selection using backward aic
step(model_break.and.enter)

###normality
ad.test(residuals(model_break.and.enter)) 
shapiro.test(residuals(model_break.and.enter)) 
lillie.test(residuals(model_break.and.enter))

###homoscedasticity
bptest(model_break.and.enter)

#####robbery#####
###model
model_robbery0<-lm(robbery~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_robbery0))
cooks.distance(model_robbery0)[cooks.distance(model_robbery0)>4/140]
sort(cooks.distance(model_robbery0))
r_robbery <- r[-c(66, 73, 75, 76, 79, 112),]
model_robbery<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_robbery)
summary(model_robbery)

###variable selection using backward aic
step(model_robbery)

###normality
ad.test(residuals(model_robbery)) 
shapiro.test(residuals(model_robbery)) 
lillie.test(residuals(model_robbery))

###homoscedasticity
bptest(model_robbery)

#####theft.over#####
###model
model_theft.over0<-lm(theft.over~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_theft.over0))
cooks.distance(model_theft.over0)[cooks.distance(model_theft.over0)>4/140]
sort(cooks.distance(model_theft.over0))
r_theft.over <- r[-c(1,21,27,31,55,75,76,121),]
model_theft.over<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_theft.over)
summary(model_theft.over)

###variable selection using backward aic
step(model_theft.over)

###normality
ad.test(residuals(model_theft.over)) 
shapiro.test(residuals(model_theft.over))
lillie.test(residuals(model_theft.over))

###homoscedasticity
bptest(model_theft.over)

#####drug.arrests#####
###model
model_drug.arrests0<-lm(drug.arrests~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_drug.arrests0))
cooks.distance(model_drug.arrests0)[cooks.distance(model_drug.arrests0)>4/140]
sort(cooks.distance(model_drug.arrests0))
r_drug.arrests <- r[-c(72,73,75,76,78,79),]
model_drug.arrests<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_drug.arrests)
summary(model_drug.arrests)

###variable selection using backward aic
step(model_drug.arrests)

###normality
ad.test(residuals(model_drug.arrests)) 
shapiro.test(residuals(model_drug.arrests)) 
lillie.test(residuals(model_drug.arrests))

###homoscedasticity
bptest(model_drug.arrests)

#####total.crime#####
###model
model_total.crime0<-lm(total.crime~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)

###outliers and influential observations
plot(cooks.distance(model_total.crime0))
cooks.distance(model_total.crime0)[cooks.distance(model_total.crime0)>4/140]
sort(cooks.distance(model_total.crime0))
r_total.crime <- r[-c(21, 73, 75, 76, 78, 79, 98, 121),]
model_total.crime<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_total.crime)
summary(model_total.crime)

###variable selection using backward aic
step(model_total.crime)

###normality
ad.test(residuals(model_total.crime)) 
shapiro.test(residuals(model_total.crime)) 
lillie.test(residuals(model_total.crime))

###homoscedasticity
bptest(model_total.crime)
