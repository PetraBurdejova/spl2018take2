r <- read.csv("agg.2016.csv")
########first model########
model.assault.first<-lm(assault~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model.assault.first)

####check for ols assumptions####
#same as for second model

########second model########
##outlier elimination and transformation
plot(density(r$assault))
library(car)
swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}
optimize(swD, c(-3,3), x=r$assault)
shapiro.test(bcPower(r$assault,0.2712042))
plot(density(bcPower(r$assault, 0.2712042)))
hist(r$assault)
boxplot(r$assault)
#outlier tests beg
#GRUBBS (ass: normal dist -> don´t use it)
library(outliers)
grubbs.test(r$assault, opposite=FALSE)
grubbs.test(r[-c(75,77),]$assault, opposite=FALSE)
#outlier tests end
r_assault<-r[-c(75,77),]
optimize(swD, c(-3,3), x=r_assault$assault)
shapiro.test(bcPower(r_assault$assault,0.2734738))
plot(density(bcPower(r_assault$assault,0.2734738)))
r_assault$assault.bp <- bcPower(r_assault$assault, 0.2734738)
model.assault<-lm(assault.bp~male.youth+less.than.high.school+low.income+immigrants, data=r_assault)
summary(model.assault)

####check for OLS-assumptions####

#Assumption: error term has a population mean of zero - irrelevant as we include an intercept
mean(model.assault$residuals)

#Assumption: no serial correlation of the error term - irrelevant as observation order is random

#Assumption: the error term is homoscedastic
library(lmtest)
bptest(model.assault) #sensitive to asumption of normality

#Assumption: error term is normally distributed
#optional: not required for OLS, but allows to performa statistical hypothesis testing and generate reliable confidence intervals
library(nortest)
shapiro.test(residuals(model.assault))
#qqplot
ad.test(residuals(model.assault))
lillie.test(residuals(model.assault))

#Assumption: regression model is linear in the coefficients and the error term
ceresPlots(model.assault)
crPlots(model.assault)

#Assumption: no (perfect) multicollinearity within the independent variables - variance inflation factors
vif(model.assault)
corrplot::corrplot(cor(r_assault[c(),]))

#Assumption: no correlation of each independent variable with the error term
cor.test(r_assault$male.youth, model.assault$residuals)

#All assumptions
library(gvlma)
gvlma(model.assault)

####further diagnosis####

#check for influential observations - cooks distance
plot(residuals(model.assault))
plot(cooks.distance(model.assault))
sort(cooks.distance(model.assault)[cooks.distance(model.assault)>4/140], decreasing=TRUE)

#variable selection - aic backwards selection
step(model.assault)
model.assault2<-lm(assault~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(model.assault2)