reg1 <- lm(Assault ~ youth, data = agg.2016)
summary(reg1)



RSS0 <- sum((agg.2016$Assault - mean(agg.2016$Assault))^2) #20181.97, this is same as TSS really
RSS <- sum(reg1$residuals^2) #20181.64
p <-  1 #predictors whos coefficient we are testing.
n <- length(agg.2016$youth) #number of observations

F <- ( (RSS0-RSS)/p ) / (RSS/(n-p-1))
F



reg2 <- lm(Total.crime ~ youth + immigrants, data = agg.2016)
summary(reg2)

df(95,1,1)

f.test(agg.2016, "Total.crime", c("youth", "immigrants"), reg2, 2)

###Can try to add density and add an if statemnt to say whethere ho is rejected or not
###Wrote an F-test function in case you want to test it on multiple columns
###or on different variables
f.test <- function(data, y, x, model.name, no.pred) {
  RSS0 <- sum((data[,y] - mean(data[, y]))^2) #20181.97, this is same as TSS really
  RSS <- sum(model.name$residuals^2) #20181.64
  p <-  no.pred #predictors whos coefficient we are testing.
  n <- nrow(data[, x]) #number of observations
  
  F <- ( (RSS0-RSS)/p ) / (RSS/(n-p-1))
  
  ##use and if statement and pf function to reject or not reject null hypothesis
  if(pf(F, p, n-p-1) >= 0.95) {
    print("You can reject the null hypothesis")
  }else{
    print("you can not reject the null hypothesis")
  }
}
