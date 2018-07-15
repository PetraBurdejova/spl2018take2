reg.data <- agg.2016[, !names(agg.2016) %in% c("Hood_ID")]
target.variables <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")
clust.variables <- c("crime.clust", "neigh.pop.char.clust", "inc.clust", "ethnicity.clust", "housing.clust", "education.clust")
per.variables <- names(agg.2016[, grepl(".per", colnames(agg.2016)), with = FALSE])

per.variables
#Find correlation between variables
var.corr <- as.data.frame(round(cor(reg.data[, !names(reg.data) %in% c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", clust.variables, per.variables)]), 2))
var.corr

plot(reg.data$renters, reg.data$total.crime)
plot(reg.data$youth, reg.data$total.crime)
plot(reg.data$low.income.pop.18.to.64, reg.data$total.crime)
plot(reg.data$unaffordable.housing, reg.data$total.crime)

#Subset variables that have an absolute correlation higher than 0.5
corr.vars <- apply(var.corr,2, function(u) row.names(var.corr)[abs(u) >= 0.5])
corr.vars <- corr.vars[order(sapply(corr.vars,length),decreasing=F)] #order variables by number of correlated variables
corr.vars <- lengths(corr.vars)
corr.vars

###Select total crime column and regressor variables
total.crime.data <- cbind.data.frame(reg.data[, "total.crime", drop = FALSE], reg.data[, !names(reg.data) %in% c(target.variables, clust.variables, per.variables), drop = FALSE]) 

##find correlation of variables between target variable and x variables
apply(total.crime.data, 2, function(col) cor(col, total.crime.data$total.crime))

###Plot varaibles with total.crime on y axis and regressors on x axis to find correlation
par(mfrow = c(3, 3))
lapply(names(total.crime.data), function(x) plot(scale(total.crime.data[[x]]), scale(total.crime.data$total.crime), xlab = x, ylab = "total.crime"))


###First regression on total crime
regressors <- c(male.youth, "lone.parent.families", "low.income", "people.ei", "non.citizens")
model.total.crime <- lm(total.crime ~  regressors  , data = total.crime.data)
summary(model.total.crime)


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model.total.crime)  # Plot the model information

# Obtain predicted and residual values for total crime  model
total.crime.data$predicted <- predict(model.total.crime)
total.crime.data$residuals <- residuals(model.total.crime)

#Create plot of x variables and total.crime variable
total.crime.data %>% 
gather(key = "iv", value = "x", refugees , male.above.15 , immigrants) %>%  # Get data into shape
  ggplot(aes(x = x, y = total.crime)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
  theme_bw()
