#code for aggregated csv file
filtered <- read.csv("filtered.csv", sep = "\t")
filtered[,15:21]<- NULL
write.csv(filtered, "filter.csv", row.names=FALSE)
rm(filtered)
filter <- read.csv("filter.csv", sep = ",")
crosssectional <- read.csv("crosssectional.csv", sep = ",")
crosssectional$Hood_ID <- as.factor(crosssectional$Hood_ID)

a<-list()
for (i in 1:140){
a[[i]] <- table(crosssectional[crosssectional$Hood_ID==i,]$MCI)
}
a <- t(as.data.frame(a))
toDelete <- seq(1, length(a), 2)
a <-  a[-toDelete, ]
aggregated <- cbind(c(1:140), a)
colnames(aggregated) <- c("Hood_ID","Assault","Auto Theft","Break and Enter","Robbery","Theft Over")
write.csv(aggregated, "aggregated.csv", row.names=FALSE)
