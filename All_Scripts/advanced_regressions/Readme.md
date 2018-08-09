[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Automated Advanced Regressions** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Automated advanced regressions


Description: Regressions for all crime types are automated in a for-loop with additional assumptions

Keywords: regression

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```


### R Code
```r

r <- as.data.frame(agg.2016)

crimetypes <- c("assault", "auto.theft", "break.and.enter", "robbery",
                "theft.over", "drug.arrests", "total.crime")

######automated advanced regression####
##log transformed data

##crimetypes and r still existent
#lists and data frames to store results of the regression loop
regressionresults.log <- list()
regressionstargazer.log <- list()
ols.ass.log <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass.log)  <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3",
                            "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass.log) <- crimetypes
ceresplots.log <- list()

##overcome the problem of log(0) = infinity 
r[, crime.var][r[, crime.var] == 0] <- 1


# loop for the regressions using original data
for (i in crimetypes){
  
  #storing the crime type in r$tmp
  r$tmp <- log(r[, i])
  
  # regression with original data
  logmodel <- lm(tmp~male.youth + less.than.high.school + low.income 
                 + immigrants, data=r)
  regressionresults.log[[i]]<-summary(logmodel)
  regressionstargazer.log[[i]] <- (logmodel)
  
  # assumption-check: the error term has a mean of zero
  ols.ass.log[i, "means"] <- mean(logmodel$residuals)
  
  # assumption-check: no serial correlation of the error term 
  # irrelevant as the observation order is random
  
  # assumption-check: the error term is homoscedastic
  ols.ass.log[i, "bptests"] <- bptest(logmodel)$p.value 
  # caution: bptest is sensitive to asumption of normality
  
  # assumption-check: the error term is normally distribute (optional)
  ols.ass.log[i, "swtests"] <- shapiro.test(residuals(logmodel))$p.value
  
  # assumption-check: no imperfect multicollinearity within the regressors
  ols.ass.log[i, "vif1"] <- vif(logmodel)[1]
  ols.ass.log[i, "vif2"] <- vif(logmodel)[2]
  ols.ass.log[i, "vif3"] <- vif(logmodel)[3]
  ols.ass.log[i, "vif4"] <- vif(logmodel)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))
  
  # assumption-check: no correlation of each regressor with the error term
  ols.ass.log[i, "cortest1"] <- cor.test(r$male.youth, 
                                         logmodel$residuals)$p.value
  ols.ass.log[i, "cortest2"] <- cor.test(r$less.than.high.school, 
                                         logmodel$residuals)$p.value
  ols.ass.log[i, "cortest3"] <- cor.test(r$low.income, 
                                         logmodel$residuals)$p.value
  ols.ass.log[i, "cortest4"] <- cor.test(r$immigrants, 
                                         logmodel$residuals)$p.value
  
  # assumption-check: linear relation between dependent variable and regressors
  crPlots(logmodel, main = paste("component + residual plots", i, "(log transformed data)"))
  ceresplots.log[[i]] <- recordPlot()
  
  rm(logmodel)
}


# 
# 
# stargazer(regressionstargazer.log,
#           dep.var.labels = crimetypes, 
#           covariate.labels = "regressionstargazer$assault$coefficients")
# ###or similar
# 

###################################################################
#####spatial regressions

#defining neighbours
shp <- readOGR("Shapefiles/Neighbourhoods_Toronto", "NEIGHBORHOODS_WGS84")
###based on queen approach
neigh <- poly2nb(shp, queen = TRUE)
W<-nb2listw(neigh, style="W", zero.policy=TRUE)
W
plot(W, coordinates(shp))
# ##based on distance 
# # coords<-coordinates(shp)
# # W_dist<-dnearneigh(coords,0,2.5,longlat = TRUE)
# ### -> check out which ones better -> Moran'S I test 
# #http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
# # moran's I test
# moran.lm <-lm.morantest(log_rob, W, alternative="two.sided")
# print(moran.lm) ## H0 <- Data ist random
# ##Lagrange multiplier test
# LM<-lm.LMtests(log_assault, W, test="all")
# print(LM)

##crimetypes and r still existent
#lists and data frames to store results of the regression loop
regressionresults.spa <- list()
regressionstargazer.spa <- list()
ols.ass.spa <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass.spa)  <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3",
                            "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass.spa) <- crimetypes
ceresplots.spa <- list()

# loop for the regressions using original data
for (i in crimetypes){
  
  #storing the crime type in r$tmp
    r$tmp <- r[,i]
  
  # regression with original data
  spamodel  <- lagsarlm(tmp~male.youth+less.than.high.school
                              +low.income+immigrants, data=r, W)
  regressionresults.spa[[i]]<-summary(spamodel)
  regressionstargazer.spa[[i]] <- (spamodel)
  
  # assumption-check: the error term has a mean of zero
  ols.ass.spa[i, "means"] <- mean(spamodel$residuals)
  
  # assumption-check: no serial correlation of the error term 
  # irrelevant as the observation order is random
  
  # assumption-check: the error term is homoscedastic
  #######Error in terms.default(formula) : no terms component nor attribute
  #ols.ass.spa[i, "bptests"] <- bptest(spamodel)$p.value 
  # caution: bptest is sensitive to asumption of normality
  
  # assumption-check: the error term is normally distribute (optional)
  ols.ass.spa[i, "swtests"] <- shapiro.test(residuals(spamodel))$p.value
  
  # assumption-check: no imperfect multicollinearity within the regressors
  ######Error in terms.default(object) : no terms component nor attribute
  # ols.ass.spa[i, "vif1"] <- vif(spamodel)[1]
  # ols.ass.spa[i, "vif2"] <- vif(spamodel)[2]
  # ols.ass.spa[i, "vif3"] <- vif(spamodel)[3]
  # ols.ass.spa[i, "vif4"] <- vif(spamodel)[4]
   #corrplot::corrplot(cor(r_assault[c(),]))
  
  # assumption-check: no correlation of each regressor with the error term
  ols.ass.spa[i, "cortest1"] <- cor.test(r$male.youth, 
                                         spamodel$residuals)$p.value
  ols.ass.spa[i, "cortest2"] <- cor.test(r$less.than.high.school, 
                                         spamodel$residuals)$p.value
  ols.ass.spa[i, "cortest3"] <- cor.test(r$low.income, 
                                         spamodel$residuals)$p.value
  ols.ass.spa[i, "cortest4"] <- cor.test(r$immigrants, 
                                         spamodel$residuals)$p.value
  
  # assumption-check: linear relation between dependent variable and regressors
  ######
  ######Error in eval(predvars, data, env) : object 'tmp' not found
  #crPlots(spamodel, main = paste("component + residual plots", i, "(spatial reg)"))
  ceresplots.spa[[i]] <- recordPlot()
  
  #rm(spamodel)
}



###################################################################
#####poisson regressions

##crimetypes and r still existent
#lists and data frames to store results of the regression loop
regressionresults.po <- list()
regressionstargazer.po <- list()
ols.ass.po <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass.po)  <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3",
                           "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass.po) <- crimetypes
ceresplots.po <- list()

# loop for the regressions using original data
for (i in crimetypes){
  
  #storing the crime type in r$tmp
  r$tmp <- r[,i]
  
  # regression with original data
  pomodel  <- glm(tmp~male.youth+less.than.high.school+low.income+immigrants, family = "poisson", data = r)
  regressionresults.po[[i]]<-summary(pomodel)
  regressionstargazer.po[[i]] <- (pomodel)
  
  # assumption-check: the error term has a mean of zero
  ols.ass.po[i, "means"] <- mean(pomodel$residuals)
  
  # assumption-check: no serial correlation of the error term 
  # irrelevant as the observation order is random
  
  # assumption-check: the error term is homoscedastic
  #######Error in terms.default(formula) : no terms component nor attribute
  #ols.ass.spa[i, "bptests"] <- bptest(spamodel)$p.value 
  # caution: bptest is sensitive to asumption of normality
  
  # assumption-check: the error term is normally distribute (optional)
  ols.ass.po[i, "swtests"] <- shapiro.test(residuals(pomodel))$p.value
  
  # assumption-check: no imperfect multicollinearity within the regressors
  ######Error in terms.default(object) : no terms component nor attribute
  # ols.ass.spa[i, "vif1"] <- vif(spamodel)[1]
  # ols.ass.spa[i, "vif2"] <- vif(spamodel)[2]
  # ols.ass.spa[i, "vif3"] <- vif(spamodel)[3]
  # ols.ass.spa[i, "vif4"] <- vif(spamodel)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))
  
  # assumption-check: no correlation of each regressor with the error term
  ols.ass.po[i, "cortest1"] <- cor.test(r$male.youth, 
                                        pomodel$residuals)$p.value
  ols.ass.po[i, "cortest2"] <- cor.test(r$less.than.high.school, 
                                        pomodel$residuals)$p.value
  ols.ass.po[i, "cortest3"] <- cor.test(r$low.income, 
                                        pomodel$residuals)$p.value
  ols.ass.po[i, "cortest4"] <- cor.test(r$immigrants, 
                                        pomodel$residuals)$p.value
  
  # assumption-check: linear relation between dependent variable and regressors
  ######
  ######Error in eval(predvars, data, env) : object 'tmp' not found
   crPlots(pomodel, main = paste("component + residual plots", i, "(poiss reg)"))
   ceresplots.po[[i]] <- recordPlot()
  
  rm(pomodel)
}

