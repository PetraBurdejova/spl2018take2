r <- read.csv("agg.2016.csv")
#crimetypes <- c("assault", "break.and.enter")
library(car)
library(lmtest)
library(nortest)
swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}
#firstregressionresults <- list()
regressionresults <- list()
ols.ass <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass) <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3", "vif4", "cortest1", "cortest2", "cortest3", "cortest4")

firstdensity <- list()
firsthistogram <- list()
density <- list()
histogram <- list()

for (i in 2:8){
  r$tmp <- r[,i]
  #firstregressionresults[[i-1]]<-summary(lm(tmp~male.youth+less.than.high.school+low.income+immigrants, data=r))
  outliers <- r[r$tmp>mean(r$tmp)+2.5*IQR(r$tmp),]$Hood_ID
  rtmp<-r[-outliers,]
  rtmp[rtmp$tmp==0,] <- 1
  exponent <- optimize(swD, c(-3,3), x=rtmp$tmp)$objective
  shapiro.test(bcPower(rtmp$tmp,exponent))
  rtmp$tmp.bp <- bcPower(rtmp$tmp, 0.2734738)
  model <- lm(tmp.bp~male.youth+less.than.high.school+low.income+immigrants, data=rtmp)
  regressionresults[[i-1]]<-summary(model)
  #Assumption: error term has a population mean of zero - irrelevant as we include an intercept
  ols.ass$means[i-1] <- mean(model$residuals)
  #Assumption: no serial correlation of the error term 
  #irrelevant as observation order is random
  #Assumption: the error term is homoscedastic
  ols.ass$bptests[i-1] <- bptest(model)$p.value #sensitive to asumption of normality
  #Assumption: error term is normally distributed (optional: not required for OLS, but allows to performa statistical hypothesis testing and generate reliable confidence intervals)
  ols.ass$swtests[i-1] <- shapiro.test(residuals(model))$p.value
  #Assumption: regression model is linear in the coefficients and the error term
#     ceresPlots(model) #crPlots(model.assault)
  #Assumption: no (perfect) multicollinearity within the independent variables - variance inflation factors
  ols.ass$vif1[i-1] <- vif(model)[1]
  ols.ass$vif2[i-1] <- vif(model)[2]
  ols.ass$vif3[i-1] <- vif(model)[3]
  ols.ass$vif4[i-1] <- vif(model)[4]
#     corrplot::corrplot(cor(r_assault[c(),]))
  #Assumption: no correlation of each independent variable with the error term
  ols.ass$cortest1[i-1] <- cor.test(rtmp$male.youth, model$residuals)$p.value
  ols.ass$cortest2[i-1] <- cor.test(rtmp$less.than.high.school, model$residuals)$p.value
  ols.ass$cortest3[i-1] <- cor.test(rtmp$low.income, model$residuals)$p.value
  ols.ass$cortest4[i-1] <- cor.test(rtmp$immigrants, model$residuals)$p.value
  firstdensity[[i-1]] <- density(r$tmp)
  firsthistogram[[i-1]] <- hist(r$tmp)
  density[[i-1]] <- density(rtmp$tmp.bp)
  histogram[[i-1]] <- hist(rtmp$tmp.bp)
  rm(exponent, model, outliers, rtmp)
  }
rm(i, swD)