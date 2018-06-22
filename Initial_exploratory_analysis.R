####Histogram of total crime commited
print(ggplot(data=agg, aes(agg$total.crime)) + #Plot distribution of crimes committed
  geom_histogram(breaks=seq(0, 1200, by = 50), col="black", fill="blue", alpha = .5) + #Set bin width to 50
  labs(title="Histogram Total Crime") + #Add title
  labs(x="Total Crime", y="Count") + #Add x and y labels
  xlim(c(0,1200)))  #Set min and max values on x label

###Histogram Assaults
print(ggplot(data=agg, aes(agg$assault)) + #Plot distribution of assaults committed
  geom_histogram(breaks=seq(0, 650, by = 25), col="black", fill="blue", alpha = .5) + #Set bin width to 25
  labs(title="Histogram Assaults") + #Add title
  labs(x="Assaults", y="Count") + #Add x and y labels
  xlim(c(0,650)))  #Set min and max values on x label

###Histogram Auto Thefts
print(ggplot(data=agg, aes(agg$auto.theft)) + #Plot distribution of autothefts
  geom_histogram(breaks=seq(0, 280, by = 20), col="black", fill="blue", alpha = .5) + #Set bin width to 20
  labs(title="Histogram Auto Thefts") + #Add title
  labs(x="Auto Thefts", y="Count") + #Add x and y labels
  xlim(c(0,300))) #Set min and max values on x label

###Histogram Break and Enters
print(ggplot(data=agg, aes(agg$break.and.enter)) + #Plot distribution of break and enters
        geom_histogram(breaks=seq(0, 200, by = 20), col="black", fill="blue", alpha = .5) + #Set bin width to 20
        labs(title="Histogram Break and Enters") + #Add title
        labs(x="Break and Enters", y="Count") + #Add x and y labels
        xlim(c(0,200))) #Set min and max values on x label

###Histogram robberies
print(ggplot(data=agg, aes(agg$robbery)) + #Plot distribution of robberies
        geom_histogram(breaks=seq(0, 140, by = 10), col="black", fill="blue", alpha = .5) + #Set bin width to 10
        labs(title="Histogram Robberies") + #Add title
        labs(x="Robberies", y="Count") + #Add x and y labels
        xlim(c(0,140))) #Set min and max values on x label

###Histogram Thefts
print(ggplot(data=agg, aes(agg$theft.over)) + #Plot distribution of Thefts
        geom_histogram(breaks=seq(0, 50, by = 5), col="black", fill="blue", alpha = .5) + #Set bin width to 5
        labs(title="Histogram Thefts") + #Add title
        labs(x="Thefts", y="Count") + #Add x and y labels
        xlim(c(0,50))) #Set min and max values on x label

###Histogram Drug Arrests
print(ggplot(data=agg, aes(agg$drug.arrests)) + #Plot distribution of drug arrests
        geom_histogram(breaks=seq(0, 180, by = 10), col="black", fill="blue", alpha = .5) + #Set bin width to 10
        labs(title="Histogram Drug Arrests") + #Add title
        labs(x="Drug Arrests", y="Count") + #Add x and y labels
        xlim(c(0,180))) #Set min and max values on x label

###Group crimes by MCI
mci.group <- group_by(a.dt, MCI)
crime.by.mci <- dplyr::summarise(mci.group, n=n()) #count of events by MCI
crime.by.mci <- crime.by.mci[order(crime.by.mci$n, decreasing = TRUE),]

plot(ggplot(aes(x = reorder(MCI, n), y = n), data = crime.by.mci) +
  geom_bar(stat = 'identity', width = 0.5) +
  geom_text(aes(label = n), stat = 'identity', data = crime.by.mci, hjust = -0.1, size = 3.5) +
  coord_flip() +
  xlab('Major Crime Indicators') +
  ylab('Number of Occurrences') +
  ggtitle('Major Crime Indicators Toronto 2016') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))

###Group crimes by time of day
hour.group <- group_by(a.dt, occurrencehour)
crime.hour <- dplyr::summarise(hour.group, n=n()) #count of crimes by hour

plot(ggplot(aes(x=occurrencehour, y=n), data = crime.hour) + geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1) + 
  geom_point(size = 0.5) + 
  ggtitle('Total Crimes by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))

###Crimes by MCI by hour
crime.type.by.hour <- group_by(a.dt, occurrencehour, MCI)
hour.crime <- dplyr::summarise(crime.type.by.hour, n=n()) #count of crime types by hour

plot(ggplot(aes(x=occurrencehour, y=n, color=MCI), data =hour.crime) + 
  geom_line(size=1.5) + 
  ggtitle('Crime Types by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))

##neighbourhoods with most crime
location.group <- group_by(a.dt, Neighbourhood)
crime.by.location <- dplyr::summarise(location.group, n=n())
crime.by.location <- crime.by.location[order(crime.by.location$n, decreasing = TRUE), ] #order neighbourhoods by crime
crime.by.location.top20 <- head(crime.by.location, 20) #top 20 neighbourhoods by crime

plot(ggplot(aes(x = reorder(Neighbourhood, n), y = n), data = crime.by.location.top20) +
  geom_bar(stat = 'identity', width = 0.6) +
  geom_text(aes(label = n), stat = 'identity', data = crime.by.location.top20, hjust = -0.1, size = 3) +
  coord_flip() +
  xlab('Neighbourhoods') +
  ylab('Number of Occurrences') +
  ggtitle('Neighbourhoods with Most Crimes - Top 20') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))

##offence types by neighbourhood
offence.location.group <- group_by(a.dt, Neighbourhood, MCI) #group crime by neighbourhod and offence
offence.type.by.location <- dplyr::summarise(offence.location.group, n=n()) #get counts of crimes by neighbourhood
offence.type.by.location <- offence.type.by.location[order(offence.type.by.location$n, decreasing = TRUE), ]
offence.type.by.location.top20 <- head(offence.type.by.location, 20)

plot(ggplot(aes(x = Neighbourhood, y=n, fill = MCI), data=offence.type.by.location.top20) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  xlab('Neighbourhood') +
  ylab('Number of Occurrence') +
  ggtitle('Offence Type vs. Neighbourhood Toronto 2016') + theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)))

##Crime count by month
crime.count <- group_by(a.dt, occurrencemonth, MCI) %>% dplyr::summarise(Total = n())
crime.count$occurrencemonth <- ordered(crime.count$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

plot(ggplot(crime.count, aes(occurrencemonth, MCI, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_gradient(
    low = "green",
    high = "red"
  )  +
  geom_text(aes(label=Total), color='black') +
  ggtitle("Major Crime Indicators by Month 2016") +
  xlab('Month') +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold")))

###Use kmeans clustering to group neighbourhoods based on crime statistics
library("cluster")
agg.kmeans <- join(neigh.codes, agg, by = "Hood_ID")
agg.kmeans <- agg.kmeans[,-c(1,2,9)]
set.seed(123)

# Test different numbers of clusters ####
# Define a vector with candidate settings for k
k.settings = 2:15

obj.values = vector(mode="numeric", length = length(k.settings))

my_kMeans <- function(data,k) {  
  clu.sol <- kmeans(data, centers=k) 
  return(clu.sol$tot.withinss)
}
obj.values <- sapply(k.settings, my_kMeans, data = agg.kmeans)


k.clust <- data.frame(k.settings, obj.values)
ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection")

kc <- kmeans(agg.kmeans, 7)
z1 <- data.frame(agg.kmeans, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Crime')
agg.2016$crime.clust <- as.factor(z1$kc.cluster)


####Use kmeans to group variables based on neighbourhood characteristics
neigh.vars <- c("male.youth", "youth", "population.2016", "density", "lone.parent.families")
agg.kmeans2 <- join(neigh.codes, agg.2016[,c("Hood_ID", neigh.vars)], by = "Hood_ID")
agg.kmeans2 <- agg.kmeans2[-c(1,2)]

obj.values <- sapply(k.settings, my_kMeans, data = agg.kmeans2)

k.clust <- data.frame(k.settings, obj.values)
ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for pop characteristics of neighbourhoods")

kc <- kmeans(agg.kmeans2, 10)
z1 <- data.frame(agg.kmeans2, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Pop Char')
agg.2016$neigh.pop.char.clust <- as.factor(z1$kc.cluster)

####Use kmeans to group variables based on neighbourhood income characteristics
inc.vars <- c("low.income", "middle.income", "high.income", "avg.income", "people.ei", "median.income",
              "no.hholds.bottom.20per", "low.income.pop")
agg.kmeans2 <- join(neigh.codes, agg.2016[,c("Hood_ID", inc.vars)], by = "Hood_ID")
agg.kmeans2 <- agg.kmeans2[-c(1,2)]

obj.values <- sapply(k.settings, my_kMeans, data = agg.kmeans2)

k.clust <- data.frame(k.settings, obj.values)
ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for income characteristics of neighbourhoods")

kc <- kmeans(agg.kmeans2, 8)
z1 <- data.frame(agg.kmeans2, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Income')
agg.2016$inc.clust <- as.factor(z1$kc.cluster)

####Use kmeans to group variables based on neighbourhood ethnicity characteristics
ethnicity.vars <- c("non.citizens", "immigrants", "refugees", "vis.minorities")
agg.kmeans2 <- join(neigh.codes, agg.2016[,c("Hood_ID", ethnicity.vars)], by = "Hood_ID")
agg.kmeans2 <- agg.kmeans2[-c(1,2)]

obj.values <- sapply(k.settings, my_kMeans, data = agg.kmeans2)

k.clust <- data.frame(k.settings, obj.values)
ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for ethnic characteristics of neighbourhoods")

kc <- kmeans(agg.kmeans2, 4)
z1 <- data.frame(agg.kmeans2, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Ethnicity')
agg.2016$ethnicity.clust <- as.factor(z1$kc.cluster)

####Use kmeans to group variables based on neighbourhood housing characteristics
house.vars <- c("houses", "hhlds.mjr.rprs")
agg.kmeans2 <- join(neigh.codes, agg.2016[,c("Hood_ID", house.vars)], by = "Hood_ID")
agg.kmeans2 <- agg.kmeans2[-c(1,2)]

obj.values <- sapply(k.settings, my_kMeans, data = agg.kmeans2)

k.clust <- data.frame(k.settings, obj.values)
ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for housing characteristics of neighbourhoods")

kc <- kmeans(agg.kmeans2, 5)
z1 <- data.frame(agg.kmeans2, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Housing')
agg.2016$housing.clust <- as.factor(z1$kc.cluster)

####Use kmeans to group variables based on neighbourhood education characteristics
education.vars <- c("less.than.high.school", "high.school.cert", "post.sec.or.above")
agg.kmeans2 <- join(neigh.codes, agg.2016[,c("Hood_ID", education.vars)], by = "Hood_ID")
agg.kmeans2 <- agg.kmeans2[-c(1,2)]

obj.values <- sapply(k.settings, my_kMeans, data = agg.kmeans2)

k.clust <- data.frame(k.settings, obj.values)
ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for education characteristics of neighbourhoods")

kc <- kmeans(agg.kmeans2, 6)
z1 <- data.frame(agg.kmeans2, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Education')
agg.2016$education.clust <- as.factor(z1$kc.cluster)

#Heatmap of toonto by population 
# Read the neighborhood shapefile data and plot
geo.data <- data.frame(agg.2016)
geo.data$Hood_ID <- str_pad(geo.data$Hood_ID, width = 3, side = 'left', pad = '0')

# the path to shape file

toronto <- readOGR(dsn = "." ,"NEIGHBORHOODS_WGS84")


# fortify and merge: muni.df is used in ggplot
toronto@data$id <- rownames(toronto@data)
toronto.geo <- fortify(toronto)
toronto.geo <- join(toronto.geo, toronto@data, by="id")
names(toronto.geo)[names(toronto.geo) == 'AREA_S_CD'] <- 'Hood_ID'

toronto.geo <- join(geo.data, toronto.geo, by = "Hood_ID")





# Plot neighbourhoods with highest population 
g.pop.2016 <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
        geom_polygon(aes(fill= population.2016)) +    # draw polygons and add fill with population variable
          geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
           coord_equal() + 
            scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                                 space = "Lab", na.value = "grey50",
                                 guide = "colourbar")+
               labs(title="Population by Neighbourhood, 2016") 
print(g.pop.2016) # render the map

# Plot neighbourhoods with highest total crime 
g.total.crime <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= total.crime)) +    # draw polygons and add fill with population variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Total crime")
print(g.total.crime) # render the map

#Plot neighbourhoods by density
g.pop.density <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= density)) +    # draw polygons and add fill with density variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444",  #Set colour scale
                      space = "Lab", na.value = "grey50", limits = c(0, 25000), #Nas are grey, set upper and lower limits of scale
                      guide = "colourbar")+ #Add colour scale on side
  labs(title="Density of Neighbourhoods") #Add title
print(g.pop.density) #Print map


#Plot neighbourhoods by robberies
g.robberies <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= robbery)) +    # draw polygons and add fill with robbery variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", #Set colour scale
                      space = "Lab", na.value = "grey50", #Na values show up as grey
                      guide = "colourbar")+ #Add colour scale at side
  labs(title="Roberries by Neighbourhood") #Add title
print(g.robberies) #print map


#Plot neighbourhoods by break and enters
g.break.n.enter <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= break.and.enter)) +    # draw polygons and add fill with break and enter variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Break and Enters by Neighbourhood")
print(g.break.n.enter)

#Plot neighbourhoods by assault
g.assault <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= assault)) +    # draw polygons and add fill with assault variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Assaults by Neighbourhood")
print(g.assault)

#Plot neighbourhoods by auto thefts
g.auto.theft <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= auto.theft)) +    # draw polygons and add fill with assault variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Auto Thefts by Neighbourhood")
print(g.auto.theft)

#Plot neighbourhoods by drug arrests
g.drug.arrests <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= drug.arrests)) +    # draw polygons and add fill with drug arrests variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Drug Arrests by Neighbourhood")
print(g.drug.arrests)

#Plot neighbourhoods by male youths
g.male.youth <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= male.youth)) +    # draw polygons and add fill with assault variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Male Youth by Neighbourhood")
print(g.male.youth)

#Plot neighbourhoods by Average Income

g.avg.income <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= avg.income, colour = "")) +    # draw polygons and add fill with assault variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      limits = c(20000, 100000),
                      labels = c("20000", "40000", "60000", "80000", "100000"),
                      space = "Lab", na.value = "#9b0a00",
                      guide = "colourbar") +
  scale_colour_manual(values = NA) +              
  guides(colour=guide_legend(">100000", override.aes = list(fill="#9b0a00"))) + 
  labs(title="Average Income by Neighbourhood")
print(g.avg.income)
