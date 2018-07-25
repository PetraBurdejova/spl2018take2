library(car)
library(lmtest)
library(nortest)

#conversion to data frame
#r <- as.data.frame(agg.2016)
r <- read.csv("agg.2016.csv")
r$obsnumber <- r$Hood_ID

#swD-function
swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}

#crimetypes vector
crimetypes <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")

#lists and data frames to store results of the regression loop
regressionresults.first <- list()
regressionresults <- list()
ols.ass <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass) <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3", "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass) <- crimetypes
ols.ass.first <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass.first) <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3", "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass.first) <- crimetypes
ceresplots <- list()
ceresplots.first <- list()


#regression loop for transformed data
for (i in crimetypes){
  
  #storing the crime type in r$tmp
  r$tmp <- r[,i]

  #outlier detection and basic power transformation
  outliers <- r[r$tmp>mean(r$tmp)+2.5*IQR(r$tmp),]$obsnumber
  rtmp<-r[!r$obsnumber %in% outliers,]
  rtmp[rtmp$tmp==0,] <- 1
  exponent <- optimize(swD, c(-3,3), x=rtmp$tmp)$objective
  shapiro.test(bcPower(rtmp$tmp,exponent))
  rtmp$tmp.bp <- bcPower(rtmp$tmp, 0.2734738)

  #regression with transformed values
  model<-lm(tmp.bp~male.youth+less.than.high.school+low.income+immigrants, data=rtmp)
  regressionresults[[i]]<-summary(model)
  
  #Assumption-check: error term has a mean of zero #irrelevant as we include an intercept
  ols.ass[i, "means"] <- mean(model$residuals)
  
  #Assumption-check: no serial correlation of the error term 
  #irrelevant as observation order is random
  
  #Assumption-check: error term is homoscedastic
  ols.ass[i, "bptests"] <- bptest(model)$p.value #sensitive to asumption of normality
  
  #Assumption-check: error term is normally distributed #optional: not required for OLS, but allows to performa statistical hypothesis testing and generate reliable confidence intervals)
  ols.ass[i, "swtests"] <- shapiro.test(residuals(model))$p.value

  #Assumption-check: no imperfect multicollinearity within the independent variables
  ols.ass[i, "vif1"] <- vif(model)[1]
  ols.ass[i, "vif2"] <- vif(model)[2]
  ols.ass[i, "vif3"] <- vif(model)[3]
  ols.ass[i, "vif4"] <- vif(model)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))
  
  #Assumption-check: no correlation of each independent variable with the error term
  ols.ass[i, "cortest1"] <- cor.test(rtmp$male.youth, model$residuals)$p.value
  ols.ass[i, "cortest2"] <- cor.test(rtmp$less.than.high.school, model$residuals)$p.value
  ols.ass[i, "cortest3"] <- cor.test(rtmp$low.income, model$residuals)$p.value
  ols.ass[i, "cortest4"] <- cor.test(rtmp$immigrants, model$residuals)$p.value

  #Assumption-check: regression model is linear in the coefficients and the error term
  crPlots(model)
  ceresplots[[i]] <- recordPlot()
  #ceresPlots(model)
  #ceresplots[[3]]
  
  rm(exponent, model, outliers, rtmp)
}


#regression loop for original data
for (i in crimetypes){

  #storing the crime type in r$tmp
  r$tmp <- r[,i]

  #first regression
  firstmodel<-lm(tmp~male.youth+less.than.high.school+low.income+immigrants, data=r)
  regressionresults.first[[i]]<-summary(firstmodel)

  #Assumption-check: error term has a mean of zero #irrelevant as we include an intercept
  ols.ass.first[i, "means"] <- mean(firstmodel$residuals)

  #Assumption-check: no serial correlation of the error term 
  #irrelevant as observation order is random

  #Assumption-check: error term is homoscedastic
  ols.ass.first[i, "bptests"] <- bptest(firstmodel)$p.value #sensitive to asumption of normality

  #Assumption-check: error term is normally distributed #optional: not required for OLS, but allows to performa statistical hypothesis testing and generate reliable confidence intervals)
  ols.ass.first[i, "swtests"] <- shapiro.test(residuals(firstmodel))$p.value

  #Assumption-check: no imperfect multicollinearity within the independent variables
  ols.ass.first[i, "vif1"] <- vif(firstmodel)[1]
  ols.ass.first[i, "vif2"] <- vif(firstmodel)[2]
  ols.ass.first[i, "vif3"] <- vif(firstmodel)[3]
  ols.ass.first[i, "vif4"] <- vif(firstmodel)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))

  #Assumption-check: no correlation of each independent variable with the error term
  ols.ass.first[i, "cortest1"] <- cor.test(r$male.youth, firstmodel$residuals)$p.value
  ols.ass.first[i, "cortest2"] <- cor.test(r$less.than.high.school, firstmodel$residuals)$p.value
  ols.ass.first[i, "cortest3"] <- cor.test(r$low.income, firstmodel$residuals)$p.value
  ols.ass.first[i, "cortest4"] <- cor.test(r$immigrants, firstmodel$residuals)$p.value

  #Assumption-check: regression model is linear in the coefficients and the error term
  crPlots(firstmodel)
  ceresplots.first[[i]] <- recordPlot()
  #ceresPlots(firstmodel)
  
  rm(firstmodel)
}

rm(crimetypes, i, swD, r)
