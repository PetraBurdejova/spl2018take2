agg <- read.csv("aggregated.csv")
class(agg$Hood_ID)
agg$Hood_ID <- as.factor(agg$Hood_ID)

char <- read.csv("wellbeing_toronto.csv")
class(char$Neighbourhood.Id)
char$Neighbourhood.Id <- as.factor(char$Neighbourhood.Id)

p <- merge(agg, char, by.x = "Hood_ID", by.y = "Neighbourhood.Id")

write.csv(p, "regressiondata.csv", row.names=FALSE)

regressiondata <- read.csv("regressiondata.csv")
regressiondata[,1] <- NULL
regressiondata$Population <- regressiondata$Population..2016
regressiondata$Population..2016 <- NULL
regressiondata$Neighbourhood <- NULL
regressiondata$X...In.Labour.Force <- NULL
regressiondata$Social.Housing.Units <- NULL

regressiondata$Pop...Males <- regressiondata$Pop...Males/regressiondata$Population
regressiondata$Youth.15.24 <- regressiondata$Youth.15.24/regressiondata$Population
regressiondata$Pop.15...64.years <- regressiondata$Pop.15...64.years/regressiondata$Population
regressiondata$Visible.Minority.Category <- regressiondata$Visible.Minority.Category/regressiondata$Population
regressiondata$X...Unemployed <- regressiondata$X...Unemployed/regressiondata$Population
regressiondata$With.Bachelor.Degree.or.Higher <- regressiondata$With.Bachelor.Degree.or.Higher/regressiondata$Population
regressiondata$Low.Income.Population <- regressiondata$Low.Income.Population/regressiondata$Population
regressiondata$Lone.Parent.Families <- regressiondata$Lone.Parent.Families/regressiondata$Population

write.csv(regressiondata, "regressiondata.csv", row.names=FALSE)
