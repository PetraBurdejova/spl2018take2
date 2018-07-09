#####REGRESSION ANALYSIS#####
r <- agg.2016
r <- read.csv("agg.2016.csv")

#####assault#####
plot(density(r$assault))
library(car)

ksD <- function (p, x) {
  y <- bcPower(x, p)
  ks.test(y, "pnorm", mean=mean(y), sd=sd(y))$statistic
} 
optimize(ksD, c(-3,3), x=r$assault) #problems with tails

swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}
optimize(swD, c(-3,3), x=r$assault)
shapiro.test(bcPower(r$assault,0.2712042))
plot(density(bcPower(r$assault, 0.2712042)))

r_ass<-r[-c(75,77),]
optimize(swD, c(-3,3), x=r_assault$assault)
shapiro.test(bcPower(r_assault$assault,0.2734738))
plot(density(bcPower(r_assault$assault,0.2734738)))

r_assault$assault.bp <- bcPower(r_assault$assault, 0.2734738)

#r_assault$male.youth <- scale(r_assault$male.youth)
#r_assault$less.than.high.school <- scale(r_assault$less.than.high.school)
#r_assault$low.income <- scale(r_assault$low.income)
#r_assault$immigrants <- scale(r_assault$immigrants)

###first model
model_assault<-lm(assault.bp~male.youth+less.than.high.school+low.income+immigrants, data=r_assault)
summary(model_assault)

###tests for normality
library(nortest)
shapiro.test(residuals(model_assault))
ad.test(residuals(model_assault))
lillie.test(residuals(model_assault))

###tests for homoscedasticity
library(lmtest)
bptest(model_assault)

#check for imperfect multicollinearity
#variance inflation factors

###influential observations - cooks distance
#plot(residuals(model_assault))
#plot(cooks.distance(model_assault))
#sort(cooks.distance(model_assault)[cooks.distance(model_assault)>4/140], decreasing=TRUE)
#r_assault <- r[-c(77,75),]
#model_assault1<-lm(assault~male.youth+less.than.high.school+low.income+immigrants, data=r_assault)
#summary(model_assault1)

###variable selection - aic backwards selection
#step(model_assault)
#model_assault2<-lm(assault~male.youth+less.than.high.school+low.income+immigrants, data=r)
#summary(model_assault2)