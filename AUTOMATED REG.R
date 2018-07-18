#####preparation for the regression loop#####
#head
r <- as.data.frame(agg.2016)
r$obsnumber <- r$Hood_ID
r$Hood_ID <- NULL
library(car)
library(lmtest)
library(nortest)
library(corrplot)

#shapiro wilk p-value for basic power transformated variable
swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}

#names of the seven regressions
crimetypes <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")

#lists and data frames to store results of the regression loop
firstregressionresults <- list()
regressionresults <- list()
ols.ass <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass) <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3", "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass) <- crimetypes
firstdensity <- list()
firsthistogram <- list()
density <- list()
histogram <- list()
ceresplots <- list()


#####regression loop#####
for (i in crimetypes){
  
  ##Outlier detection, basic power transformation and regression
  r$tmp <- r[,i]
  firstregressionresults[[i]]<-summary(lm(tmp~male.youth+less.than.high.school+low.income+immigrants, data=r))
  outliers <- r[r$tmp>mean(r$tmp)+2.5*IQR(r$tmp),]$obsnumber
  rtmp<-r[!r$obsnumber %in% outliers,]
  rtmp[rtmp$tmp==0,] <- 1
  exponent <- optimize(swD, c(-3,3), x=rtmp$tmp)$objective
  shapiro.test(bcPower(rtmp$tmp,exponent))
  rtmp$tmp.bp <- bcPower(rtmp$tmp, 0.2734738)
  model <- lm(tmp.bp~male.youth+less.than.high.school+low.income+immigrants, data=rtmp)
  regressionresults[[i]]<-summary(model)
  
  ##Check for OLS-Assumptions
  
  #Assumption: error term has a population mean of zero - irrelevant as we include an intercept
  ols.ass[i, "means"] <- mean(model$residuals)
  
  #Assumption: no serial correlation of the error term 
  #irrelevant as observation order is random
  
  #Assumption: the error term is homoscedastic
  ols.ass[i, "bptests"] <- bptest(model)$p.value #sensitive to asumption of normality
  
  #Assumption: error term is normally distributed (optional: not required for OLS, but allows to performa statistical hypothesis testing and generate reliable confidence intervals)
  ols.ass[i, "swtests"] <- shapiro.test(residuals(model))$p.value
  
  #Assumption: regression model is linear in the coefficients and the error term
  crPlots(model)
  ceresplots[[i]] <- recordPlot() #ceresPlots(model)
  
  #Assumption: no (perfect) multicollinearity within the independent variables - variance inflation factors
  ols.ass[i, "vif1"] <- vif(model)[1]
  ols.ass[i, "vif2"] <- vif(model)[2]
  ols.ass[i, "vif3"] <- vif(model)[3]
  ols.ass[i, "vif4"] <- vif(model)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))
  
  #Assumption: no correlation of each independent variable with the error term
  ols.ass[i, "cortest1"] <- cor.test(rtmp$male.youth, model$residuals)$p.value
  ols.ass[i, "cortest2"] <- cor.test(rtmp$less.than.high.school, model$residuals)$p.value
  ols.ass[i, "cortest3"] <- cor.test(rtmp$low.income, model$residuals)$p.value
  ols.ass[i, "cortest4"] <- cor.test(rtmp$immigrants, model$residuals)$p.value
  firstdensity[[i]] <- density(r$tmp)
  firsthistogram[[i]] <- hist(r$tmp)
  density[[i]] <- density(rtmp$tmp.bp)
  histogram[[i]] <- hist(rtmp$tmp.bp)
  rm(exponent, model, outliers, rtmp)
}

rm(crimetypes, i, swD)