#####REGRESSION ANALYSIS#####
r <- agg.2016

#####assault#####

###first model
model_assault<-lm(assault~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_assault)

###influential observations - cooks distance
plot(residuals(model_assault))
plot(cooks.distance(model_assault))
sort(cooks.distance(model_assault)[cooks.distance(model_assault)>4/140], decreasing=TRUE)
r_assault <- r[-c(77,75,131,51,73),]
model_assault1<-lm(assault~male.youth+less.than.high.school+low.income+immigrants, data=r_assault)
summary(model_assault1)

###variable selection - aic backwards selection
step(model_assault)
model_assault2<-lm(assault~male.youth+less.than.high.school, data=r)
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
model_auto.theft<-lm(auto.theft~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_auto.theft)

###influential observations - cooks distance
plot(residuals(model_auto.theft))
plot(cooks.distance(model_auto.theft))
sort(cooks.distance(model_auto.theft)[cooks.distance(model_auto.theft)>4/140], decreasing=TRUE)
r_auto.theft <- r[-c(1,14),]
model_auto.theft1<-lm(auto.theft~male.youth+less.than.high.school+low.income+immigrants, data=r_auto.theft)
summary(model_auto.theft1)

###variable selection - aic backwards selection
step(model_auto.theft)
model_auto.theft2<-lm(auto.theft~male.youth+less.than.high.school+low.income+immigrants, data=r)
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
model_break.and.enter<-lm(break.and.enter~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_break.and.enter)

###influential observations - cooks distance
plot(residuals(model_break.and.enter))
plot(cooks.distance(model_break.and.enter))
sort(cooks.distance(model_break.and.enter)[cooks.distance(model_break.and.enter)>4/140], decreasing=TRUE)
r_break.and.enter <- r[-c(77,137,39,73,75),]
model_break.and.enter1<-lm(break.and.enter~male.youth+less.than.high.school+low.income+immigrants, data=r_break.and.enter)
summary(model_break.and.enter1)

###variable selection - aic backwards selection
step(model_break.and.enter)
model_break.and.enter2<-lm(break.and.enter~male.youth, data=r)
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
model_robbery<-lm(robbery~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_robbery)

###influential observations - cooks distance
plot(residuals(model_robbery))
plot(cooks.distance(model_robbery))
sort(cooks.distance(model_robbery)[cooks.distance(model_robbery)>4/140], decreasing=TRUE)
r_robbery <- r[-c(75,73,9,2),]
model_robbery1<-lm(robbery~male.youth+less.than.high.school+low.income+immigrants, data=r_robbery)
summary(model_robbery1)

###variable selection - aic backwards selection
step(model_robbery)
model_robbery2<-lm(robbery~male.youth+low.income+immigrants, data=r)
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
model_theft.over<-lm(theft.over~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_theft.over)

###influential observations - cooks distance
plot(residuals(model_theft.over))
plot(cooks.distance(model_theft.over))
sort(cooks.distance(model_theft.over)[cooks.distance(model_theft.over)>4/140], decreasing = TRUE)
r_theft.over <- r[-c(77,14),]
model_theft.over1<-lm(theft.over~male.youth+less.than.high.school+low.income+immigrants, data=r_theft.over)
summary(model_theft.over1)

###variable selection - aic backwards selection
step(model_theft.over)
model_theft.over2<-lm(theft.over~male.youth+less.than.high.school, data=r)
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
model_drug.arrests<-lm(drug.arrests~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_drug.arrests)

###influential observations - cooks distance
plot(residuals(model_drug.arrests))
plot(cooks.distance(model_drug.arrests))
sort(cooks.distance(model_drug.arrests)[cooks.distance(model_drug.arrests)>4/140], decreasing=TRUE)
r_drug.arrests <- r[-c(77,73,76,75,78),]
model_drug.arrests1<-lm(drug.arrests~male.youth+less.than.high.school+low.income+immigrants, data=r_drug.arrests)
summary(model_drug.arrests1)

###variable selection - aic backwards selection
step(model_drug.arrests)
model_drug.arrests2<-lm(drug.arrests~male.youth, data=r)
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
model_total.crime<-lm(total.crime~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model_total.crime)

###influential observations - cooks distance
plot(residuals(model_total.crime))
plot(cooks.distance(model_total.crime))
sort(cooks.distance(model_total.crime)[cooks.distance(model_total.crime)>4/140], decreasing=TRUE)
r_total.crime <- r[-c(77,75,73,131,51,137),]
model_total.crime1<-lm(total.crime~male.youth+less.than.high.school+low.income+immigrants, data=r_total.crime)
summary(model_total.crime1)

###variable selection - aic backwards selection
step(model_total.crime)
model_total.crime2<-lm(total.crime~male.youth, data=r)
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
