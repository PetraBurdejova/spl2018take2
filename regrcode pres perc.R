#####agg.2016 -> r#####
r <- agg.2016
rm(list=setdiff(ls(), "r"))

r$assault <- r$assault/r$population.2016
r$auto.theft <- r$auto.theft/r$population.2016
r$break.and.enter <- r$break.and.enter/r$population.2016
r$robbery <- r$robbery/r$population.2016
r$theft.over <- r$theft.over/r$population.2016
r$drug.arrests <- r$drug.arrests/r$population.2016
r$total.crime <- r$total.crime/r$population.2016

r$youth.perc <- (r$youth/r$population.2016)*100 
r$youth <- NULL
r$male.youth.perc <- (r$male.youth/r$population.2016)*100 
r$male.youth <- NULL
r$male.above.15 <- (r$male.above.15/r$population.2016)*100 
r$male.above.15 <- NULL
r$female.above.15 <- (r$female.above.15/r$population.2016)*100 
r$female.above.15 <- NULL
r$lone.parent.families.perc <- r$lone.parent.families.perc*100
r$lone.parent.families <- NULL
r$low.income.perc <- (r$low.income/r$population.2016)*100
r$low.income <- NULL
r$middle.income.perc <- (r$middle.income/r$population.2016)*100
r$middle.income <- NULL
#colnames(r)[11] <-"high.income"
r$high.income.perc <- (r$high.income/r$population.2016)*100
r$high.income <- NULL

r$people.ei <- NULL
r$people.ei.per <- r$people.ei.per*100
#r$median.income
r$no.hholds.bottom.20per <- NULL
r$hholds.bottom.20per.per <- NULL
r$low.income.pop <- NULL
r$low.income.pop.18.to.64 <- NULL
r$non.citizens <- NULL
r$non.citizens.perc <- r$non.citizens.perc*100
r$immigrants <- NULL
r$immigrants.perc <- r$immigrants.perc*100
r$immigrants.recent <- NULL
r$immigrants.recent.perc <- r$immigrants.recent.perc*100
r$refugees <- NULL
r$refugees.perc <- r$refugees.perc*100
r$vis.minorities <- NULL
r$vis.minorities.perc <- r$vis.minorities.perc*100
r$renters <- NULL
r$renters.perc <- r$renters.perc*100
r$houses <- NULL
r$houses.perc <- r$houses.perc*100
r$unsuitable.housing <- NULL
r$unsuitable.housing.perc <- r$unsuitable.housing.perc*100
r$hhlds.mjr.rprs <- NULL
r$hhlds.mjr.rprs.perc <- r$hhlds.mjr.rprs.perc*100
r$unaffordable.housing <- NULL
r$unaffordale.housing.perc <- r$unaffordale.housing.perc*100
#colnames(r)[colnames(r)=="unaffordale.housing.perc"] <-"unaffordable.housing.perc"
r$less.than.high.school <- NULL
r$less.than.high.school.perc <- r$less.than.high.school.perc*100
r$high.school.cert <- NULL
r$high.school.cert.perc <- r$high.school.cert*100
r$post.sec.or.above <- NULL
r$post.sec.or.above.perc <- r$post.sec.or.above.perc*100
r$unemployed <- NULL
r$unemployed.males <- NULL
#r <- round(r, digits = 3)
#write.csv(r, "regressiondata.csv", row.names=FALSE)

#####variable preselection#####
library(PerformanceAnalytics) #for chart.Correlation
chart.Correlation(r[,9:35], histogram=TRUE)
cortable <- cor(r[,10:37])
cortablefiltered <- ifelse(cortable>0.5, cortable, NA)
#median.income
#r$avg.income <- NULL
#r$people.ei.per <- NULL
#r$low.income.pop.perc <- NULL
#r$low.income.pop.perc.18.to.64 <- NULL
#r$low.income.perc <- NULL
#r$middle.income.perc <- NULL
#r$high.income.perc <- NULL
#r$unsuitable.housing.perc <- NULL 
#r$hhlds.mjr.rprs.perc <- NULL
#r$unaffordable.housing.perc <- NULL
#r$houses.perc <- NULL
#immigrants.perc
#r$non.citizens.perc <- NULL
#r$immigrants.recent.perc <- NULL
#r$refugees.perc <- NULL
#r$vis.minorities.perc <- NULL
#less.than.high.school.perc
#r$high.school.cert.perc <- NULL
#r$post.sec.or.above.perc <- NULL
#unemployment.rate.males
#r$unemployment.rate <- NULL
#male.youth.perc
#r$youth.perc <- NULL
#
#r$lone.parent.families.perc <- NULL
#r$renters.perc <- NULL
#chart.Correlation(r[,9:14], histogram=TRUE)
#cortable <- cor(r[,10:15])
#cortablefiltered <- ifelse(cortable>0.5, cortable, NA)

#median.income+immigrants.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc

#####REGRESSION ANALYSIS#####
#shortcut
#r <- read.csv("regressiondata.csv")
#####assault#####

###first model
model_assault<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_assault)

###influential observations - cooks distance
plot(residuals(model_assault))
plot(cooks.distance(model_assault))
cooks.distance(model_assault)[cooks.distance(model_assault0)>4/140]
r_assault <- r[-c(35, 73, 75, 76, 78, 79, 83, 86),]
model_assault1<-lm(assault~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_assault)
summary(model_assault1)

###variable selection - aic backwards selection
step(model_assault)
model_assault2<-lm(assault~houses.perc+male.youth.perc, data=r)
summary(model_assault2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_assault))
ad.test(residuals(model_assault))
lillie.test(residuals(model_assault))

###tests for homoscedasticity
library(lmtest)
bptest(model_assault)

#####auto.theft#####

###first model
model_auto.theft<-lm(auto.theft~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_auto.theft)

###influential observations - cooks distance
plot(residuals(model_auto.theft))
plot(cooks.distance(model_auto.theft))
cooks.distance(model_auto.theft)[cooks.distance(model_auto.theft)>4/140]
r_auto.theft <- r[-c(1,14,76),]
model_auto.theft1<-lm(auto.theft~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_auto.theft)
summary(model_auto.theft1)

###variable selection - aic backwards selection
step(model_auto.theft)
model_auto.theft2<-lm(immigrants.perc+auto.theft~houses.perc+male.youth.perc, data=r)
summary(model_auto.theft2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_auto.theft))
ad.test(residuals(model_auto.theft))
lillie.test(residuals(model_auto.theft))

###tests for homoscedasticity
library(lmtest)
bptest(model_auto.theft)

#####break.and.enter#####

###first model
model_break.and.enter<-lm(break.and.enter~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_break.and.enter)

###influential observations - cooks distance
plot(residuals(model_break.and.enter))
plot(cooks.distance(model_break.and.enter))
cooks.distance(model_break.and.enter)[cooks.distance(model_break.and.enter)>4/140]
r_break.and.enter <- r[-c(35, 41, 66, 67, 70, 73, 78, 79),]
model_break.and.enter1<-lm(break.and.enter~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_break.and.enter)
summary(model_break.and.enter1)

###variable selection - aic backwards selection
step(model_break.and.enter)
model_break.and.enter2<-lm(break.and.enter~1, data=r)
summary(model_break.and.enter2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_break.and.enter))
ad.test(residuals(model_break.and.enter))
lillie.test(residuals(model_break.and.enter))

###tests for homoscedasticity
library(lmtest)
bptest(model_break.and.enter)

#####robbery#####

###first model
model_robbery<-lm(robbery~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_robbery)

###influential observations - cooks distance
plot(residuals(model_robbery))
plot(cooks.distance(model_robbery))
cooks.distance(model_robbery)[cooks.distance(model_robbery)>4/140]
r_robbery <- r[-c(35, 66, 73, 75, 78, 79, 83, 86, 106),]
model_robbery1<-lm(robbery~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_robbery)
summary(model_robbery1)

###variable selection - aic backwards selection
step(model_robbery)
model_robbery2<-lm(robbery~median.income+immigrants.perc+houses.perc+male.youth.perc, data=r)
summary(model_robbery2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_robbery))
ad.test(residuals(model_robbery))
lillie.test(residuals(model_robbery))

###tests for homoscedasticity
library(lmtest)
bptest(model_robbery)

#####theft.over#####

###first model
model_theft.over<-lm(theft.over~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_theft.over)

###influential observations - cooks distance
plot(residuals(model_theft.over))
plot(cooks.distance(model_theft.over))
cooks.distance(model_theft.over)[cooks.distance(model_theft.over)>4/140]
r_theft.over <- r[-c(1, 21, 27, 31, 66, 70, 75, 76, 78, 117),]
model_theft.over1<-lm(theft.over~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_theft.over)
summary(model_theft.over1)

###variable selection - aic backwards selection
step(model_theft.over)
model_theft.over2<-lm(theft.over~houses.perc+male.youth.perc, data=r)
summary(model_theft.over2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_theft.over))
ad.test(residuals(model_theft.over))
lillie.test(residuals(model_theft.over))

###tests for homoscedasticity
library(lmtest)
bptest(model_theft.over)

#####drug.arrests#####

###first model
model_drug.arrests<-lm(drug.arrests~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_drug.arrests)

###influential observations - cooks distance
plot(residuals(model_drug.arrests))
plot(cooks.distance(model_drug.arrests))
cooks.distance(model_drug.arrests)[cooks.distance(model_drug.arrests)>4/140]
r_drug.arrests <- r[-c(34, 35, 73, 76, 78, 79, 81, 83, 113),]
model_drug.arrests1<-lm(drug.arrests~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_drug.arrests)
summary(model_drug.arrests1)

###variable selection - aic backwards selection
step(model_drug.arrests)
model_drug.arrests2<-lm(drug.arrests~houses.perc+less.than.high.school.perc+male.youth.perc, data=r)
summary(model_drug.arrests2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_drug.arrests))
ad.test(residuals(model_drug.arrests))
lillie.test(residuals(model_drug.arrests))

###tests for homoscedasticity
library(lmtest)
bptest(model_drug.arrests)

#####total.crime#####

###first model
model_total.crime<-lm(total.crime~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r)
summary(model_total.crime)

###influential observations - cooks distance
plot(residuals(model_total.crime))
plot(cooks.distance(model_total.crime))
cooks.distance(model_total.crime)[cooks.distance(model_total.crime)>4/140]
r_total.crime <- r[-c(35, 66, 67, 73, 75, 76, 78, 79, 83, 86),]
model_total.crime1<-lm(total.crime~median.income+immigrants.perc+houses.perc+less.than.high.school.perc+unemployment.rate.males+male.youth.perc, data=r_total.crime)
summary(model_total.crime1)

###variable selection - aic backwards selection
step(model_total.crime)
model_total.crime2<-lm(total.crime~median.income+immigrants.perc+houses.perc+male.youth.perc, data=r)
summary(model_total.crime2)

#check for imperfect multicollinearity
#variance inflation factors

###tests for normality
library(nortest)
shapiro.test(residuals(model_total.crime))
ad.test(residuals(model_total.crime))
lillie.test(residuals(model_total.crime))

###tests for homoscedasticity
library(lmtest)
bptest(model_total.crime)
