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
library(nortest)
shapiro.test(residuals(log_assault)) ### H0 <- normality ----> p>0.05 ---> normality

lillie.test(residuals(log_assault)) ### same as above -> normally distributed



####test for heteroscedasticity
library(lmtest)
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
library(nortest)
shapiro.test(residuals(log_rob)) ### H0 <- normality ----> p>0.05 ---> normality

lillie.test(residuals(log_rob)) ### same as above -> normally distributed



####test for heteroscedasticity
library(lmtest)
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



library(ggplot2)

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

install.packages("stargazer")
library(stargazer)

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








