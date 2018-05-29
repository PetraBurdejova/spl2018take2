if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE); library("tidyverse")
if(!require("xts")) install.packages("xts", dependencies = TRUE); library("xts")
if(!require("zoo")) install.packages("zoo", dependencies = TRUE); library("zoo")
if(!require("chron")) install.packages("chron", dependencies = TRUE); library("chron")
if(!require("forecast")) install.packages("forecast", dependencies = TRUE); library("forecast")
if(!require("reshape2")) install.packages("reshape2", dependencies = TRUE); library("reshape2")

setwd("C:/Users/Felix/Desktop/Uni/Berlin/2. Semester/SPL")



a <- read.csv("MCI_2014_to_2017.csv")
b <- read.csv("2016_neighbourhood_profiles.csv")


df1 <- as.data.frame(b)
df1 <- b[-c(1,2),-c(1,3)]
str(df1)

###Create a dataframe of codes for each neighbourhood
neigh.codes <- as.data.frame(cbind(colnames(df1[,-c(1,2)]), as.vector(unlist(b[1,-c(1:4)]))))
colnames(neigh.codes) <- c("Neighborhood", "Hood_ID")

#####Remove thousands seperator commas from numbers and replace % sign eith e-2, 
####so we can use as.numeric to convert from a character to a number
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], function(x) {gsub(",", "", x)})
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], function(x) {gsub("%", "e-2", x)})

###Turn n/as into NA
df1[df1 == "n/a"] <- NA

####Turn columns into numeric
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], as.numeric)

###Turn data from wide to long format using melt
df2 <- melt(df1, id.vars=c("Topic", "Characteristic"))
colnames(df2) <- c("Topic", "Characteristic", "Neighborhood", "Value")

###Merge hood_ids to dataframe
df2 <- merge(df2, neigh.codes, by.x = "Neighborhood", by.y = "Neighborhood")
str(df2)

a$Hood_ID <- as.factor(a$Hood_ID)
str(a)

###This is a test to see how the two datasets can be merged, can only do a few characteristics as a time, otherwise
###R freezes
test <- df2[which( df2$Characteristic == "Youth (15-24 years)" | df2$Characteristic == "Worked at usual place"),]
df3 <- merge(a, test[,-c(1,2)], by.x = "Hood_ID", by.y = "Hood_ID")

##funktioniert bis hier 
df3 <- merge(a, df2, by.x = "Hood_ID", by.y = "Hood_ID")



df2 <- df1 %>%
  group_by(Characteristic) %>%
  mutate(id = 1:n()) %>%
  spread(Characteristic, Value)

         