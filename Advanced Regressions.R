#######basic stuff######
####assault#####
hist(agg.2016$assault)
plot(density(agg.2016$assault))

agg.2016$log.assault <- log(agg.2016$assault)

hist(agg.2016$log.assault)
plot(density(agg.2016$log.assault))

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



###tests for normality
library(nortest)
shapiro.test(residuals(log_assault)) ### H0 <- normality ----> p>0.05 ---> normality

lillie.test(residuals(log_assault)) ### same as above -> normally distributed


####test for heteroscedasticity
library(lmtest)
bptest(log_assault) ### H0 <- homoscedasticity no heteroscedasticity 
##achtung! very sensitive to violation of normal assumption


####spatial regression####


#defining neighbours
shp <- readOGR(".", "NEIGHBORHOODS_WGS84")
###based on queen approach
neigh <- poly2nb(shp, queen = TRUE)
W<-nb2listw(neigh, style="W", zero.policy=TRUE)
W
plot(W, coordinates(shp))

##based on distance 
coords<-coordinates(shp)
W_dist<-dnearneigh(coords,0,2.5,longlat = TRUE)
### -> check out which ones better -> Moran'S I test 

#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html


ols <- lm(robbery ~ youth +greenarea, data = agg.2016)
summary(ols)
#### in such a specification, greenarea is highly significant. But then, moran.lm cannot be rejected


# moran's I test
moran.lm <-lm.morantest(ols, W, alternative="two.sided")
print(moran.lm)

##Lagrange multiplier test
LM<-lm.LMtests(ols, W, test="all")
print(LM)


sar<-lagsarlm(robbery ~ youth + greenarea, data = agg.2016, W)
summary(sar)
summary(ols)

#not very high differences 


#poisson regression 

summary(m1 <- glm(robbery ~ youth + greenarea + non.citizens + lone.parent.families, family = "poisson", data = agg.2016))

summary(ols_log <- lm(logrobbery ~ youth +greenarea, data = agg.2016))










