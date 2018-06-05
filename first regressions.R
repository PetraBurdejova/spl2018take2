library(data.table)
library(reshape2)
library(forcats)
library(dplyr)
#####create agg#####

#read csvs
a <- read.csv("MCI_2014_to_2017.csv")
b <- read.csv("2016_neighbourhood_profiles.csv")

#Create aggregate data frame using data table
a.dt <- as.data.table(a)
setkey(a.dt, "MCI", "Hood_ID")
agg <- a.dt[, .(count = .N), by = c("MCI", "Hood_ID")]
agg <- dcast(agg, Hood_ID ~ MCI)
agg$Hood_ID <- as.factor(agg$Hood_ID)

#b --> df1
df1 <- as.data.frame(b)
df1 <- b[-c(1,2),-c(1,2,3,5)]

###Data frame with Hood IDs
neigh.codes <- as.data.frame(cbind(colnames(df1[,-1]), as.vector(unlist(b[1,-c(1:5)]))))
colnames(neigh.codes) <- c("Neighborhood", "Hood_ID")

###remove strange characters in dataframe 
df1[,-1] <- lapply(df1[,-1], function(x) {gsub(",", "", x)})
df1[,-1] <- lapply(df1[,-1], function(x) {gsub("%", "e-2", x)})
df1[df1 == "n/a"] <- NA
df1[,-1] <- lapply(df1[,-1], as.numeric)
str(df1)

####HERE
df1 <- fct_collapse(df1$Characteristic,
                    "young_male" = c("Male: 15 to 19 years", "Male: 20 to 24 years", "Male: 25 to 29 years",
                                   "Male: 30 to 34 years", "Male: 35 to 39 years"))




get.data <- function(x, z, y) {
  m <- as.data.frame(cbind(unlist(x[x$Characteristic == y, -c(1:2)]), neigh.codes))
  m$Neighborhood <- NULL
  colnames(m) <- c(y, "Hood_ID")
  p <- merge(m,z, by.x = "Hood_ID", by.y = "Hood_ID")
  return(p)
}

#add varaibles to agg
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
agg$Unemployed.y <- agg$Unemployed.y/agg$`Population, 2016`

#####first regressions#####
model<-lm(Robbery~Unemployed.y,data=agg)
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