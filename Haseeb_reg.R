reg.data <- agg.2016[, !names(agg.2016) %in% c("Hood_ID")]
target.variables <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")
clust.variables <- c("crime.clust", "neigh.pop.char.clust", "inc.clust", "ethnicity.clust", "housing.clust", "education.clust")
per.variables <- names(agg.2016[, grepl(".per", colnames(agg.2016))])

#Find correlation between variables
var.corr <- as.data.frame(round(cor(reg.data[, !names(reg.data) %in% c(target.variables, clust.variables, per.variables)]), 2))
var.corr


#Subset variables that have an absolute correlation higher than 0.5
corr.vars <- apply(var.corr,2, function(u) row.names(var.corr)[abs(u) >= 0.5])
corr.vars <- corr.vars[order(sapply(corr.vars,length),decreasing=F)] #order variables by number of correlated variables
corr.vars <- lengths(corr.vars)
corr.vars

total.crime.data <- reg.data %>% select(total.crime, ) 
###First regression on total crime
model.total.crime <- lm(total.crime ~ . -assault -auto.theft -break.and.enter -robbery -theft.over -drug.arrests, data = reg.data)
summary(model.total.crime)


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model.total.crime)  # Plot the model information
