#######basic stuff######
####assault#####
hist(agg.2016$assault)
plot(density(agg.2016$assault))

agg.2016$log.assault <- log(agg.2016$assault)

hist(agg.2016$log.assault)
plot(density(agg.2016$log.assault))

####robbery#####
hist(agg.2016$robbery)
plot(density(agg.2016$robbery))
agg.2016$robbery[agg.2016$robbery == 0]
agg.2016$robbery[agg.2016$robbery == 0] <- 1

agg.2016$log.rob <- log(agg.2016$robbery)

hist(agg.2016$log.rob)
plot(density(agg.2016$log.rob))




###try first models####

r <- agg.2016

log_assault <- lm(log.assault~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(log_assault)

log_plot <- plot(log_assault)

# Residuals versus fitted values (for checking E(ε|X) = 0)  
# 
# QQ plot: ordered residuals versus normal quantiles (for checking normality).
# 
# Scale-location plot:  √ˆri| (of standardized residuals ri ) versus fitted valuesˆyi (for checking i.i.d. assumption, in particular  Var (ε|X) =σ2I).
# 
# Combinations of standardized residuals, leverage, and Cook’s
# distance


plot(residuals(log_assault))
plot(density(residuals(log_assault)))

0.0002

summary(log_assault)


###tests for normality

shapiro.test(residuals(log_assault)) ### H0 <- normality ----> p>0.05 ---> normality

lillie.test(residuals(log_assault)) ### same as above -> normally distributed



####test for heteroscedasticity

bptest(log_assault) ### H0 <- homoscedasticity no heteroscedasticity 
##achtung! very sensitive to violation of normal assumption

####robbery#####


log_rob <- lm(log.rob~male.youth+less.than.high.school+low.income+immigrants, data=r)
summary(log_rob)

log_plot <- plot(log_rob)

# Residuals versus fitted values (for checking E(ε|X) = 0)  
# 
# QQ plot: ordered residuals versus normal quantiles (for checking normality).
# 
# Scale-location plot:  √ˆri| (of standardized residuals ri ) versus fitted valuesˆyi (for checking i.i.d. assumption, in particular  Var (ε|X) =σ2I).
# 
# Combinations of standardized residuals, leverage, and Cook’s
# distance


plot(residuals(log_rob))
plot(density(residuals(log_rob)))



###tests for normality
shapiro.test(residuals(log_rob)) ### H0 <- normality ----> p>0.05 ---> normality

lillie.test(residuals(log_rob)) ### same as above -> normally distributed



####test for heteroscedasticity
bptest(log_rob) ### H0 <- homoscedasticity no heteroscedasticity 
##achtung! very sensitive to violation of normal assumption

####further transform variables#####


agg.2016$rob.cap <- agg.2016$robbery / agg.2016$population.2016

plot(density(agg.2016$rob.cap))
plot(agg.2016$rob.cap)

agg.2016$log.rob.cap <- log(agg.2016$rob.cap)
plot(density(agg.2016$log.rob.cap))
plot(agg.2016$log.rob.cap)

agg.2016$male.youth.per <- agg.2016$male.youth / agg.2016$population.2016
agg.2016$male.youth.cap <- agg.2016$male.youth / agg.2016$population.2016
agg.2016$male.youth.cap <- agg.2016$male.youth / agg.2016$population.2016


summary(model2 <- lm(rob.cap~ male.youth.per, data=agg.2016))


m <- agg.2016
m$pred <- predict(model2)
m$res <- residuals(model2)

plot(density(m$res))

plot(m$pred)

plot(m$rob.cap, add = T, col = "RED")




ggplot(m, aes(x = male.youth.per, y = rob.cap)) +  # Set up canvas with outcome variable on y-axis
  geom_point() +
  geom_point(aes(y = pred), shape = 3)


ggplot(m, aes(x = male.youth.per, y = rob.cap)) +
  geom_segment(aes(xend = male.youth.per, yend = pred)) +
  geom_point() +
  geom_point(aes(y = pred), shape = 1)


ggplot(m, aes(x = male.youth.per, y = rob.cap)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + 
  geom_segment(aes(xend = male.youth.per, yend = pred), alpha = .2) +  
  geom_point() +
  geom_point(aes(y = pred), shape = 1) +
  theme_bw()


ggplot(m, aes(x = male.youth.per, y = rob.cap)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = male.youth.per, yend = pred), alpha = .2) +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(res), size = abs(res))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # <
  
  geom_point(aes(y = pred), shape = 1) +
  theme_bw()



####spatial regression####
#defining neighbours
shp <- readOGR(".", "NEIGHBORHOODS_WGS84")
###based on queen approach
neigh <- poly2nb(shp, queen = TRUE)
W<-nb2listw(neigh, style="W", zero.policy=TRUE)
W
plot(W, coordinates(shp))

##based on distance 
# coords<-coordinates(shp)
# W_dist<-dnearneigh(coords,0,2.5,longlat = TRUE)
### -> check out which ones better -> Moran'S I test 

#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html


# moran's I test
moran.lm <-lm.morantest(log_rob, W, alternative="two.sided")
print(moran.lm) ## H0 <- Data ist random

##Lagrange multiplier test
LM<-lm.LMtests(log_assault, W, test="all")
print(LM)


spatial_log_rob  <-lagsarlm(log.rob~male.youth+less.than.high.school+low.income+immigrants, data=r, W)
summary(spatial_log_rob)
summary(log_rob)

#not very high differences 


#poisson regression 

poisson_rob <- glm(robbery~male.youth+less.than.high.school+low.income+immigrants, family = "poisson", data = r)
summary(poisson_rob)
summary(spatial_log_rob)
summary(log_rob)

plot(residuals(poisson_rob))

plot(residuals(spatial_log_rob))
plot(residuals(log_rob))



stargazer(model_robbery, log_rob, spatial_log_rob, poisson_rob,
          dep.var.labels = c("Robbery", "Robbery", "Robbery"), 
          covariate.labels = c("Male youth", "Less than High School", "Low Income Households", "Visible immigrants"), out = "models.tex")


#####additional spatial regressionnn######
neighbourhood.center <- st_centroid(toronto_map$geometry)
###hoodID 77 has most assaults -> calculate distance from every neighbourhood to neighbourhood 77

toronto_map1 <- readOGR(".", "NEIGHBORHOODS_WGS84")
toronto_small <- subset(toronto_map1, toronto_map1@data$AREA_S_CD == "077")

knn1 <- knn( coordinates(toronto_small), coordinates(toronto_map1), k=1)
knn.dist <- knn1$nn.dists

r$dist <- knn.dist














######automated regression####


#conversion to data frame
r <- as.data.frame(agg.2016)
r$obsnumber <- r$Hood_ID

#swD-function
swD <- function (p, x) {
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}

#crimetypes vector
crimetypes <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")

#lists and data frames to store results of the regression loop
regressionresults <- list()
regressionstargazer.log <- list()
ols.ass.log <- as.data.frame(matrix(nrow=7, ncol=11))
colnames(ols.ass.log) <- c("means", "bptests", "swtests", "vif1", "vif2", "vif3", "vif4", "cortest1", "cortest2", "cortest3", "cortest4")
rownames(ols.ass.log) <- crimetypes
ceresplots.log <- list()



#regression loop for original data
for (i in crimetypes){
  
  #storing the crime type in r$tmp
  r[,i][r[,i] == 0] <- 1
  r$tmp <- r[,i]
  
  #first regression
  log.model<-lm(log(tmp)~male.youth+less.than.high.school+low.income+immigrants, data=r)
  regressionresults.log[[i]]<-summary(log.model)
  regressionstargazer.log[[i]] <- log.model
  
  #Assumption-check: error term has a mean of zero #irrelevant as we include an intercept
  ols.ass.log[i, "means"] <- mean(log.model$residuals)
  
  #Assumption-check: no serial correlation of the error term 
  #irrelevant as observation order is random
  
  #Assumption-check: error term is homoscedastic
  ols.ass.log[i, "bptests"] <- bptest(log.model)$p.value #sensitive to asumption of normality
  
  #Assumption-check: error term is normally distributed #optional: not required for OLS, but allows to performa statistical hypothesis testing and generate reliable confidence intervals)
  ols.ass.log[i, "swtests"] <- shapiro.test(residuals(log.model))$p.value
  
  #Assumption-check: no imperfect multicollinearity within the independent variables
  ols.ass.log[i, "vif1"] <- vif(log.model)[1]
  ols.ass.log[i, "vif2"] <- vif(log.model)[2]
  ols.ass.log[i, "vif3"] <- vif(log.model)[3]
  ols.ass.log[i, "vif4"] <- vif(log.model)[4]
  #corrplot::corrplot(cor(r_assault[c(),]))
  
  #Assumption-check: no correlation of each independent variable with the error term
  ols.ass.log[i, "cortest1"] <- cor.test(r$male.youth, log.model$residuals)$p.value
  ols.ass.log[i, "cortest2"] <- cor.test(r$less.than.high.school, log.model$residuals)$p.value
  ols.ass.log[i, "cortest3"] <- cor.test(r$low.income, log.model$residuals)$p.value
  ols.ass.log[i, "cortest4"] <- cor.test(r$immigrants, log.model$residuals)$p.value
  
  #Assumption-check: regression model is linear in the coefficients and the error term
  crPlots(log.model)
  ceresplots.log[[i]] <- recordPlot()
  #ceresPlots(firstmodel)
  
  rm(log.model)
}

rm(crimetypes, i, swD, r)





stargazer(regressionstargazer.log,
          dep.var.labels = crimetypes, 
          covariate.labels = "regressionstargazer$assault$coefficients")
###or similar









