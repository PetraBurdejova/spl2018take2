reg1 <- lm(Assault ~ youth, data = agg.2016)
summary(reg1)


RSS0 <- sum((agg.2016$Assault - mean(agg.2016$Assault))^2) #20181.97, this is same as TSS really
RSS <- sum(reg1$residuals^2) #20181.64
p <-  1 #predictors whos coefficient we are testing.
n <- length(agg.2016$youth) #number of observations

F <- ( (RSS0-RSS)/p ) / (RSS/(n-p-1))
F
