#####read csv#####
regressiondata <- read.csv("regressiondata.csv")
r <- regressiondata
#dependent variables (dont run it)
r$Assault
r$Auto.Theft
r$Break.and.Enter
r$Robbery
r$Theft.Over
#independent variables (dont run it)
r$lone.parent.families.perc
r$avg.income
r$low.income.pop.perc
r$non.citizens.perc
r$immigrants.recent.perc
r$refugees.perc
r$vis.minorities.perc
r$renters.perc
r$houses.perc
r$less.than.high.school.perc
r$high.school.cert.perc
r$post.sec.or.above.perc
r$unemployment.rate
r$youth.perc


#####regression models#####
model_Assault<-lm(Assault~lone.parent.families.perc+avg.income+low.income.pop.perc+non.citizens.perc+immigrants.recent.perc+refugees.perc+vis.minorities.perc+
            renters.perc+houses.perc+less.than.high.school.perc+high.school.cert.perc+post.sec.or.above.perc+unemployment.rate+youth.perc, data=r)

model_Auto.Theft<-lm(Auto.Theft~lone.parent.families.perc+avg.income+low.income.pop.perc+non.citizens.perc+immigrants.recent.perc+refugees.perc+vis.minorities.perc+
             renters.perc+houses.perc+less.than.high.school.perc+high.school.cert.perc+post.sec.or.above.perc+unemployment.rate+youth.perc, data=r)

model_Break.and.Enter<-lm(Break.and.Enter~lone.parent.families.perc+avg.income+low.income.pop.perc+non.citizens.perc+immigrants.recent.perc+refugees.perc+vis.minorities.perc+
             renters.perc+houses.perc+less.than.high.school.perc+high.school.cert.perc+post.sec.or.above.perc+unemployment.rate+youth.perc, data=r)

model_Robbery<-lm(Robbery~lone.parent.families.perc+avg.income+low.income.pop.perc+non.citizens.perc+immigrants.recent.perc+refugees.perc+vis.minorities.perc+
             renters.perc+houses.perc+less.than.high.school.perc+high.school.cert.perc+post.sec.or.above.perc+unemployment.rate+youth.perc, data=r)

model_Theft.Over<-lm(Theft.Over~lone.parent.families.perc+avg.income+low.income.pop.perc+non.citizens.perc+immigrants.recent.perc+refugees.perc+vis.minorities.perc+
             renters.perc+houses.perc+less.than.high.school.perc+high.school.cert.perc+post.sec.or.above.perc+unemployment.rate+youth.perc, data=r)

#####Assault#####
summary(model_Assault)

###outliers and influential observations
plot(x=r$Hood_ID,y=residuals(model_Assault),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
sort(model_Assault$residuals)[c(1,140)]
plot(cooks.distance(model_Assault))
cooks.distance(model_Assault)[cooks.distance(model_Assault)>4/140]
plot(dffits(model_Assault))
dffits(model_Assault)[dffits(model_Assault)>2*sqrt(14/140)]
plot(rstudent(model_Assault))

###variable selection
#backward selection using aic
step(model_Assault)
#backward selection using bic
step(model_Assault, k=log(140))
#backward selection using significance of coefficients

###test for multicollinearity
cor(r$avg.income, r$youth.perc)
chart.Correlation(r[,8:21], histogram=TRUE)


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
