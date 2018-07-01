####Histogram of total crime commited
print(ggplot(data=agg, aes(agg$total.crime)) + #Plot distribution of crimes committed
  geom_histogram(breaks=seq(0, 1200, by = 50), col="black", fill="blue", alpha = .5) + #Set bin width to 50
  labs(title="Histogram Total Crime") + #Add title
  labs(x="Total Crime", y="Count") + #Add x and y labels
  xlim(c(0,1200)))  #Set min and max values on x label

ggsave("plots_and_images/hist_total_crime.png", width=10, height=5, dpi=150)

###Histogram Assaults
print(ggplot(data=agg, aes(agg$assault)) + #Plot distribution of assaults committed
  geom_histogram(breaks=seq(0, 650, by = 25), col="black", fill="blue", alpha = .5) + #Set bin width to 25
  labs(title="Histogram Assaults") + #Add title
  labs(x="Assaults", y="Count") + #Add x and y labels
  xlim(c(0,650)))  #Set min and max values on x label

ggsave("plots_and_images/hist_assaults.png", width=10, height=5, dpi=150)

###Histogram Auto Thefts
print(ggplot(data=agg, aes(agg$auto.theft)) + #Plot distribution of autothefts
  geom_histogram(breaks=seq(0, 280, by = 20), col="black", fill="blue", alpha = .5) + #Set bin width to 20
  labs(title="Histogram Auto Thefts") + #Add title
  labs(x="Auto Thefts", y="Count") + #Add x and y labels
  xlim(c(0,300))) #Set min and max values on x label

ggsave("plots_and_images/hist_auto_thefts.png", width=10, height=5, dpi=150)

###Histogram Break and Enters
print(ggplot(data=agg, aes(agg$break.and.enter)) + #Plot distribution of break and enters
        geom_histogram(breaks=seq(0, 200, by = 20), col="black", fill="blue", alpha = .5) + #Set bin width to 20
        labs(title="Histogram Break and Enters") + #Add title
        labs(x="Break and Enters", y="Count") + #Add x and y labels
        xlim(c(0,200))) #Set min and max values on x label

ggsave("plots_and_images/hist_break_n_enters.png", width=10, height=5, dpi=150)

###Histogram robberies
print(ggplot(data=agg, aes(agg$robbery)) + #Plot distribution of robberies
        geom_histogram(breaks=seq(0, 140, by = 10), col="black", fill="blue", alpha = .5) + #Set bin width to 10
        labs(title="Histogram Robberies") + #Add title
        labs(x="Robberies", y="Count") + #Add x and y labels
        xlim(c(0,140))) #Set min and max values on x label

ggsave("plots_and_images/hist_robberies.png", width=10, height=5, dpi=150)

###Histogram Thefts
print(ggplot(data=agg, aes(agg$theft.over)) + #Plot distribution of Thefts
        geom_histogram(breaks=seq(0, 50, by = 5), col="black", fill="blue", alpha = .5) + #Set bin width to 5
        labs(title="Histogram Thefts") + #Add title
        labs(x="Thefts", y="Count") + #Add x and y labels
        xlim(c(0,50))) #Set min and max values on x label

ggsave("plots_and_images/hist_thefts.png", width=10, height=5, dpi=150)

###Histogram Drug Arrests
print(ggplot(data=agg, aes(agg$drug.arrests)) + #Plot distribution of drug arrests
        geom_histogram(breaks=seq(0, 180, by = 10), col="black", fill="blue", alpha = .5) + #Set bin width to 10
        labs(title="Histogram Drug Arrests") + #Add title
        labs(x="Drug Arrests", y="Count") + #Add x and y labels
        xlim(c(0,180))) #Set min and max values on x label

ggsave("plots_and_images/hist_drug_arrests.png", width=10, height=5, dpi=150)

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

ggsave("plots_and_images/crimes_by_MCI.png", width=12, height=5, dpi=150)

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

ggsave("plots_and_images/crime_by_hour.png", width=10, height=5, dpi=150)

###Crimes by MCI by hour
crime.type.by.hour <- group_by(a.dt, occurrencehour, MCI)
hour.crime <- dplyr::summarise(crime.type.by.hour, n=n()) #count of crime types by hour

plot(ggplot(aes(x=occurrencehour, y=n, color=MCI, linetype = MCI), data =hour.crime) + 
  geom_line(size=1.5) + 
  ggtitle('Crime Types by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))

ggsave("plots_and_images/crime_by_MCI_by_hour.png", width=10, height=5, dpi=150)

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

ggsave("plots_and_images/most_crime_neighbhoods.png", width=10, height=5, dpi=150)

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

ggsave("plots_and_images/crime_type_by_neighbhood.png", width=10, height=5, dpi=150)

##Crime count by month
crime.count <- group_by(a.dt, occurrencemonth, MCI) %>% dplyr::summarise(Total = n())
crime.count$occurrencemonth <- ordered(crime.count$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

plot(ggplot(crime.count, aes(occurrencemonth, MCI, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_gradient(
    low = "#7ff4f0",
    high = "#000c8c"
  )  +
  geom_text(aes(label=Total), color='black') +
  ggtitle("Major Crime Indicators by Month 2016") +
  xlab('Month') +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold")))


ggsave("plots_and_images/crime_by_MCI_by_month.png", width=10, height=5, dpi=150)

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
plot(ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection using crime statistics"))

ggsave("plots_and_images/elbow_curve_crime_stats.png", width=10, height=5, dpi=150)

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
plot(ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for pop characteristics of neighbourhoods"))

ggsave("plots_and_images/elbow_curve_pop_char.png", width=10, height=5, dpi=150)

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
plot(ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for income characteristics of neighbourhoods"))

ggsave("plots_and_images/elbow_curve_income_char.png", width=10, height=5, dpi=150)

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
plot(ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for ethnic characteristics of neighbourhoods"))

ggsave("plots_and_images/elbow_curve_income_char.png", width=10, height=5, dpi=150)

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
plot(ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for housing characteristics of neighbourhoods"))

ggsave("plots_and_images/elbow_curve_house_char.png", width=10, height=5, dpi=150)

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
plot(ggplot(k.clust, aes(k.settings,obj.values))+
  geom_line(color = "red") + 
  geom_point(color="red")  + 
  xlab("k") + ylab("Total within-cluster SS") + 
  ggtitle("Elbow curve for k selection for education characteristics of neighbourhoods"))

ggsave("plots_and_images/elbow_curve_edu_char.png", width=10, height=5, dpi=150)

kc <- kmeans(agg.kmeans2, 6)
z1 <- data.frame(agg.kmeans2, kc$cluster)

clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis Education')
agg.2016$education.clust <- as.factor(z1$kc.cluster)

#Heatmap of toonto by population 
# Read the neighborhood shapefile data and plot
geo.data <- data.frame(agg.2016)
geo.data$Hood_ID <- str_pad(geo.data$Hood_ID, width = 3, side = 'left', pad = '0')

###above command does not work#######





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
          geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
           coord_equal() + 
            scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                                 space = "Lab", na.value = "grey50",
                                 guide = "colourbar")+
               labs(title="Population by Neighbourhood, 2016") 
print(g.pop.2016) # render the map

ggsave("plots_and_images/heat_map_pop.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by density
g.pop.density <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= density, colour = "")) +    # draw polygons and add fill with density variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c",  #Set colour scale
                      space = "Lab", na.value = "#000647", limits = c(0, 25000), #Nas are grey, set upper and lower limits of scale
                      guide = "colourbar") + #Add colour scale on side
  scale_colour_manual(values = NA) +              
  guides(colour=guide_legend(">25000", override.aes = list(fill="#000647"))) +
  labs(title="Density of Neighbourhoods") #Add title
print(g.pop.density) #Print map

ggsave("plots_and_images/heat_map_density.png", width=10, height=5, dpi=150)

# Plot neighbourhoods with total crime 
g.total.crime <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= total.crime)) +    # draw polygons and add fill with population variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar") +
  labs(title="Total crime")
print(g.total.crime) # render the map

ggsave("plots_and_images/heat_map_total_crime.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by robberies
g.robberies <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= robbery)) +    # draw polygons and add fill with robbery variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", #Set colour scale
                      space = "Lab", na.value = "grey50", #Na values show up as grey
                      guide = "colourbar")+ #Add colour scale at side
  labs(title="Roberries by Neighbourhood") #Add title
print(g.robberies) #print map

ggsave("plots_and_images/heat_map_robberies.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by break and enters
g.break.n.enter <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= break.and.enter)) +    # draw polygons and add fill with break and enter variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Break and Enters by Neighbourhood")
print(g.break.n.enter)

ggsave("plots_and_images/heat_map_break_n_enter.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by assault
g.assault <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= assault)) +    # draw polygons and add fill with assault variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Assaults by Neighbourhood")
print(g.assault)

ggsave("plots_and_images/heat_map_assault.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by auto thefts
g.auto.theft <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= auto.theft)) +    # draw polygons and add fill with assault variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Auto Thefts by Neighbourhood")
print(g.auto.theft)

ggsave("plots_and_images/heat_map_auto_theft.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by drug arrests
g.drug.arrests <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= drug.arrests)) +    # draw polygons and add fill with drug arrests variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Drug Arrests by Neighbourhood")
print(g.drug.arrests)

ggsave("plots_and_images/heat_map_drug_arrests.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by male youths
g.male.youth <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= male.youth)) +    # draw polygons and add fill with assault variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Male Youth by Neighbourhood")
print(g.male.youth)

ggsave("plots_and_images/heat_map_male_youths.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by Average Income
g.avg.income <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= avg.income, colour = "")) +    # draw polygons and add fill with avg income variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      limits = c(20000, 100000),
                      labels = c("20000", "40000", "60000", "80000", "100000"),
                      space = "Lab", na.value = "#000647",
                      guide = "colourbar") +
  scale_colour_manual(values = NA) +              
  guides(colour=guide_legend(">100000", override.aes = list(fill="#000647"))) + 
  labs(title="Average Income by Neighbourhood")
print(g.avg.income)

ggsave("plots_and_images/heat_map_avg_income.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by Median Income
g.median.income <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= median.income)) +    # draw polygons and add fill with medianincome variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Median Income by Neighbourhood")
print(g.median.income)

ggsave("plots_and_images/heat_map_med_income.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of households in bottom 20 percent
g.hholds.bottom.20per.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= hholds.bottom.20per.per)) +    # draw polygons and add fill with medianincome variable
  geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of Households in Bottom 20% by Neighbourhood")
print(g.hholds.bottom.20per.per)

ggsave("plots_and_images/heat_map_per_hholds_bottom20per.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of people in low income bracket
g.low.income.pop.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= low.income.pop.per)) +    # draw polygons and add fill with medianincome variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of People Classified as Low-income by Neighbourhood")
print(g.low.income.pop.per)

ggsave("plots_and_images/heat_map_per_lowincome.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of immigrants
g.immigrants.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= immigrants.per)) +    # draw polygons and add fill with immigrants percentage variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of People Classified as Immigrants by Neighbourhood")
print(g.immigrants.per)

ggsave("plots_and_images/heat_map_per_immigrants.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of visible minorities
g.vis.minorities.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= vis.minorities.per)) +    # draw polygons and add fill with immigrants percentage variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of People Classified as Visible Minorities by Neighbourhood")
print(g.vis.minorities.per)

ggsave("plots_and_images/heat_map_per_vis_min.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of renters
g.renters.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= renters.per)) +    # draw polygons and add fill with renters percentage variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of People Renting by Neighbourhood")
print(g.renters.per)

ggsave("plots_and_images/heat_map_per_renters.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of households that require majour repairs
g.hhlds.mjr.rprs.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= hhlds.mjr.rprs.per)) +    # draw polygons and add fill with percentage of households requiring majour repairs variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of Households that Require Majour Repairs")
print(g.hhlds.mjr.rprs.per)

ggsave("plots_and_images/heat_map_per_hhlds_mjrrprs.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of households spending 30% or more of income on housing
g.unaffordable.housing.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= unaffordable.housing.per)) +    # draw polygons and add fill with percentage of households spending more than 30% of income on rent variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Percentage of Households Spending More than 30% of Income on Rent")
print(g.unaffordable.housing.per)

ggsave("plots_and_images/heat_map_per_unaff_hsing.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by percentage of people with highschool certificate or less
g.high.school.or.less.per <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= less.than.high.school.per + high.school.cert.per)) +    # draw polygons and add fill with percentage of households spending more than 30% of income on rent variable
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar") +
  labs(title="Percentage of People with High School Cert or Less", fill = "high school or less")
print(g.high.school.or.less.per)

ggsave("plots_and_images/heat_map_per_hghschl_orless.png", width=10, height=5, dpi=150)

#Plot neighbourhoods by unemployment rate
g.unemployment.rate <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= unemployment.rate)) +    # draw polygons and add fill with unemployment rate
  geom_path(color="light grey") +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#7ff4f0", high = "#000c8c", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar") +
  labs(title="Unemployment Rate by Neighbourhood", fill = "unemployment rate")
print(g.unemployment.rate)

ggsave("plots_and_images/heat_map_unemplymnt_rate.png", width=10, height=5, dpi=150)
