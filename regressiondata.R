regressiondata <- read.csv("regressiondata0.csv")

regressiondata$Assault <- regressiondata$Assault/regressiondata$population.2016
regressiondata$Auto.Theft <- regressiondata$Auto.Theft/regressiondata$population.2016
regressiondata$Break.and.Enter <- regressiondata$Break.and.Enter/regressiondata$population.2016
regressiondata$Robbery <- regressiondata$Robbery/regressiondata$population.2016
regressiondata$Theft.Over <- regressiondata$Theft.Over/regressiondata$population.2016
regressiondata$Drug.Arrests <- regressiondata$Drug.Arrests/regressiondata$population.2016
regressiondata$Total.crime <- regressiondata$Total.crime/regressiondata$population.2016

regressiondata$youth.perc <- (regressiondata$youth/regressiondata$population.2016)*100 
regressiondata$youth <- NULL
regressiondata$male.youth.perc <- (regressiondata$male.youth/regressiondata$population.2016)*100 
regressiondata$male.youth <- NULL
regressiondata$male.above.15 <- (regressiondata$male.above.15/regressiondata$population.2016)*100 
regressiondata$male.above.15 <- NULL
regressiondata$female.above.15 <- (regressiondata$female.above.15/regressiondata$population.2016)*100 
regressiondata$female.above.15 <- NULL
regressiondata$lone.parent.families.perc <- regressiondata$lone.parent.families.perc*100
regressiondata$lone.parent.families <- NULL
regressiondata$low.income.perc <- (regressiondata$low.income/regressiondata$population.2016)*100
regressiondata$low.income <- NULL
regressiondata$middle.income.perc <- (regressiondata$middle.income/regressiondata$population.2016)*100
regressiondata$middle.income <- NULL
colnames(regressiondata)[11] <-"high.income"
regressiondata$high.income.perc <- (regressiondata$high.income/regressiondata$population.2016)*100
regressiondata$high.income <- NULL

regressiondata$people.ei <- NULL
regressiondata$people.ei.per <- regressiondata$people.ei.per*100
regressiondata$median.income
regressiondata$no.hholds.bottom.20per <- NULL
regressiondata$hholds.bottom.20per.per <- NULL
regressiondata$low.income.pop <- NULL
regressiondata$low.income.pop.18.to.64 <- NULL
regressiondata$non.citizens <- NULL
regressiondata$non.citizens.perc <- regressiondata$non.citizens.perc*100
regressiondata$immigrants <- NULL
regressiondata$immigrants.perc <- regressiondata$immigrants.perc*100
regressiondata$immigrants.recent <- NULL
regressiondata$immigrants.recent.perc <- regressiondata$immigrants.recent.perc*100
regressiondata$refugees <- NULL
regressiondata$refugees.perc <- regressiondata$refugees.perc*100
regressiondata$vis.minorities <- NULL
regressiondata$vis.minorities.perc <- regressiondata$vis.minorities.perc*100
regressiondata$renters <- NULL
regressiondata$renters.perc <- regressiondata$renters.perc*100
regressiondata$houses <- NULL
regressiondata$houses.perc <- regressiondata$houses.perc*100
regressiondata$unsuitable.housing <- NULL
regressiondata$unsuitable.housing.perc <- regressiondata$unsuitable.housing.perc*100
regressiondata$hhlds.mjr.rprs <- NULL
regressiondata$hhlds.mjr.rprs.perc <- regressiondata$hhlds.mjr.rprs.perc*100
regressiondata$unaffordable.housing <- NULL
regressiondata$unaffordale.housing.perc <- regressiondata$unaffordale.housing.perc*100
colnames(regressiondata)[colnames(regressiondata)=="unaffordale.housing.perc"] <-"unaffordable.housing.perc"
regressiondata$less.than.high.school <- NULL
regressiondata$less.than.high.school.perc <- regressiondata$less.than.high.school.perc*100
regressiondata$high.school.cert <- NULL
regressiondata$high.school.cert.perc <- regressiondata$high.school.cert*100
regressiondata$post.sec.or.above <- NULL
regressiondata$post.sec.or.above.perc <- regressiondata$post.sec.or.above.perc*100
regressiondata$unemployed <- NULL
regressiondata$unemployed.males <- NULL
#regressiondata <- round(regressiondata, digits = 3)

write.csv(regressiondata, "regressiondata.csv", row.names=FALSE)