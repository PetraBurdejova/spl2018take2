[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Automated regression** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Automated regression


Description: Regressions for all crime types are automated in a for-loop

Keywords: regression

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```


### R Code
```r
source("Merging.R")

# conversion of agg.2016 to a data frame
r <- as.data.frame(agg.2016)
r$obsnumber <- r$Hood_ID

# create the FindBestExponent-function
FindBestExponent <- function (p, x) {
  # finds the best exponent for a basic power transformation towards normality
  # arguments:
  # p: the exponent used for the basic power transformation
  # x: the data we want to transform
  # returns:
  # the p-value of the Shapiro-Wilk test applied on the transformed data
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}

# create a character vector containing all crimetypes
crimetypes <- c("assault", "auto.theft", "break.and.enter", "robbery",
                "theft.over", "drug.arrests", "total.crime")

# create lists and data frames to store the results of the regression loop in
regressionresults.first <- list()
regressionresults <- list()
ols.ass <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass) <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3",
                       "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass) <- crimetypes
ols.ass.first <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass.first) <- c("means", "bptests", "swtests", "vif1", "vif2",
                             "vif3", "vif4","cortest1", "cortest2", "cortest3",
                             "cortest4")
rownames(ols.ass.first) <- crimetypes
ceresplots <- list()
ceresplots.first <- list()


# loop for the regressions using transformed data
for (i in crimetypes){
  
  # storing the crime type in a temporary column
  r$tmp <- r[,i]

  # outlier detection and basic power transformation
  outliers <- r[r$tmp > mean(r$tmp)+2.5*IQR(r$tmp), ]$obsnumber
  rtmp <- r[!r$obsnumber %in% outliers, ]
  rtmp[rtmp$tmp == 0, ] <- 1
  exponent <- optimize(FindBestExponent, c(-3,3), x=rtmp$tmp)$objective
  shapiro.test(bcPower(rtmp$tmp,exponent))
  rtmp$tmp.bp <- bcPower(rtmp$tmp, 0.2734738)

  # regression with transformed data
  model <- lm(tmp.bp~male.youth + less.than.high.school + low.income 
              + immigrants, data=rtmp)
  regressionresults[[i]]<-summary(model)
  
  # assumption-check: the error term has a mean of zero
  ols.ass[i, "means"] <- mean(model$residuals)
  
  # assumption-check: no serial correlation of the error term 
  # irrelevant as the observation order is random
  
  # assumption-check: the error term is homoscedastic
  ols.ass[i, "bptests"] <- bptest(model)$p.value
  # caution: bptest is sensitive to asumption of normality
  
  # assumption-check: the error term is normally distributed (optional)
  ols.ass[i, "swtests"] <- shapiro.test(residuals(model))$p.value

  # assumption-check: no imperfect multicollinearity within the regressors
  ols.ass[i, "vif1"] <- vif(model)[1]
  ols.ass[i, "vif2"] <- vif(model)[2]
  ols.ass[i, "vif3"] <- vif(model)[3]
  ols.ass[i, "vif4"] <- vif(model)[4]
  
  # assumption-check: no correlation of each regressor with the error term
  ols.ass[i, "cortest1"] <- cor.test(rtmp$male.youth, model$residuals)$p.value
  ols.ass[i, "cortest2"] <- cor.test(rtmp$less.than.high.school, 
                                     model$residuals)$p.value
  ols.ass[i, "cortest3"] <- cor.test(rtmp$low.income, model$residuals)$p.value
  ols.ass[i, "cortest4"] <- cor.test(rtmp$immigrants, model$residuals)$p.value

  # assumption-check: linear relation between dependent variable and regressors
  crPlots(model)
  ceresplots[[i]] <- recordPlot()

  rm(exponent, model, outliers, rtmp)
}


# loop for the regressions using original data
for (i in crimetypes){

  # storing the crime type in a temporary column
  r$tmp <- r[,i]

  # regression with original data
  firstmodel <- lm(tmp~male.youth + less.than.high.school + low.income 
                   + immigrants, data=r)
  regressionresults.first[[i]]<-summary(firstmodel)

  # assumption-check: the error term has a mean of zero
  ols.ass.first[i, "means"] <- mean(firstmodel$residuals)

  # assumption-check: no serial correlation of the error term 
  # irrelevant as the observation order is random

  # assumption-check: the error term is homoscedastic
  ols.ass.first[i, "bptests"] <- bptest(firstmodel)$p.value 
  # caution: bptest is sensitive to asumption of normality

  # assumption-check: the error term is normally distribute (optional)
  ols.ass.first[i, "swtests"] <- shapiro.test(residuals(firstmodel))$p.value

  # assumption-check: no imperfect multicollinearity within the regressors
  ols.ass.first[i, "vif1"] <- vif(firstmodel)[1]
  ols.ass.first[i, "vif2"] <- vif(firstmodel)[2]
  ols.ass.first[i, "vif3"] <- vif(firstmodel)[3]
  ols.ass.first[i, "vif4"] <- vif(firstmodel)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))

  # assumption-check: no correlation of each regressor with the error term
  ols.ass.first[i, "cortest1"] <- cor.test(r$male.youth, 
                                           firstmodel$residuals)$p.value
  ols.ass.first[i, "cortest2"] <- cor.test(r$less.than.high.school, 
                                           firstmodel$residuals)$p.value
  ols.ass.first[i, "cortest3"] <- cor.test(r$low.income, 
                                           firstmodel$residuals)$p.value
  ols.ass.first[i, "cortest4"] <- cor.test(r$immigrants, 
                                           firstmodel$residuals)$p.value

  # assumption-check: linear relation between dependent variable and regressors
  crPlots(firstmodel)
  ceresplots.first[[i]] <- recordPlot()
  
  rm(firstmodel)
}


