#read csv
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


#regression
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

summary(model_Assault)
summary(model_Auto.Theft)
summary(model_Break.and.Enter)
summary(model_Robbery)
summary(model_Theft.Over)


#plot errors
plot(x=r$Hood_ID,y=residuals(model_Assault),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
plot(x=r$Hood_ID,y=residuals(model_Auto.Theft),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
plot(x=r$Hood_ID,y=residuals(model_Break.and.Enter),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
plot(x=r$Hood_ID,y=residuals(model_Robbery),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))
plot(x=r$Hood_ID,y=residuals(model_Theft.Over),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))


#autocorrelation
library(car)
dwt(model_Assault)
dwt(model_Auto.Theft)
dwt(model_Break.and.Enter)
dwt(model_Robbery)
dwt(model_Theft.Over)


#normailty
library(nortest)
ad.test(residuals(model)) 
shapiro.test(residuals(model)) 
lillie.test(residuals(model))
