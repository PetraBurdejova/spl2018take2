#####create agg#####
#b -> df1
b <- read.csv("2016_neighbourhood_profiles.csv")
df1 <- b[-c(1,2),-c(1,3)]
neigh.codes <- as.data.frame(cbind(colnames(df1[,-c(1,2)]), as.vector(unlist(b[1,-c(1:4)]))))
colnames(neigh.codes) <- c("Neighborhood", "Hood_ID")
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], function(x) {gsub(",", "", x)})
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], function(x) {gsub("%", "e-2", x)})
df1[df1 == "n/a"] <- NA
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], as.numeric)
rm(b)

#agg
agg <- read.csv("aggregated.csv")
get.data <- function(x, z, y) {
  m <- as.data.frame(cbind(unlist(x[x$Characteristic == y, -c(1:2)]), neigh.codes$Hood_ID))
  colnames(m) <- c(y, "Hood_ID")
  p <- merge(m,z, by.x = "Hood_ID", by.y = "Hood_ID")
  return(p)
}
y <- c("Population, 2016", "Unemployed")
for(i in y){
  agg <- get.data(df1, agg, i)
}

#adjust for population size
agg$Assault <- agg$Assault/agg$`Population, 2016`
agg$Auto.Theft <- agg$Auto.Theft/agg$`Population, 2016`
agg$Break.and.Enter <- agg$Break.and.Enter/agg$`Population, 2016`
agg$Robbery <- agg$Robbery/agg$`Population, 2016`
agg$Theft.Over <- agg$Theft.Over/agg$`Population, 2016`
agg$Unemployed <- agg$Unemployed/agg$`Population, 2016`

#####run regressions on agg#####
model<-lm(Robbery~Unemployed,data=agg)
summary(model)

#plot errors
plot(x=agg$Hood_ID,y=residuals(model),xlab="Hood", ylab="Residuals",panel.last = abline(h=0, lty=2))

#autocorrelation
library(car)
dwt(model)

#normailty
library(nortest)
ad.test(residuals(model)) 
shapiro.test(residuals(model)) 
lillie.test(residuals(model))
