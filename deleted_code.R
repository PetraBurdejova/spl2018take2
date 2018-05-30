
###Turn data from wide to long format using melt
df2 <- melt(df1, id.vars=c("Topic", "Characteristic"))
colnames(df2) <- c("Topic", "Characteristic", "Neighborhood", "Value")

###Merge hood_ids to dataframe
df2 <- merge(df2, neigh.codes, by.x = "Neighborhood", by.y = "Neighborhood")
str(df2)

a$Hood_ID <- as.factor(a$Hood_ID)
str(a)

a <- read.csv("aggregated.csv")

###This is a test to see how the two datasets can be merged, can only do a few characteristics as a time, otherwise
###R freezes
test <- df2[which( df2$Characteristic == "Youth (15-24 years)" | df2$Characteristic == "Worked at usual place"),]
df3 <- merge(a, j, by.x = "Hood_ID", by.y = "Hood_ID")
df4 <- merge(df3, i, by.x = "Hood_ID", by.y = "Hood_ID")
# 
# ##funktioniert bis hier 
# df3 <- merge(a, df2, by.x = "Hood_ID", by.y = "Hood_ID")
# 
# 
# 
# df2 <- df1 %>%
#   group_by(Characteristic) %>%
#   mutate(id = 1:n()) %>%
#   spread(Characteristic, Value)
# 
#          