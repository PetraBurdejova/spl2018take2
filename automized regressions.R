r <- read.csv("agg.2016.csv")
#crimetypes <- c("assault", "break.and.enter")
library(car)
swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}
regressionresults <- list()
for (i in 2:8){
  r$tmp <- r[,i]
  outliers <- r[r$tmp>mean(r$tmp)+2.5*IQR(r$tmp),]$Hood_ID
  rtmp<-r[-outliers,]
  rtmp[rtmp$tmp==0,] <- 1
  exponent <- optimize(swD, c(-3,3), x=rtmp$tmp)$objective
  shapiro.test(bcPower(rtmp$tmp,exponent))
  rtmp$tmp.bp <- bcPower(rtmp$tmp, 0.2734738)
  regressionresults[[i]]<-summary(lm(tmp.bp~male.youth+less.than.high.school+low.income+immigrants, data=rtmp))
}
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
 
  #storing! 
}