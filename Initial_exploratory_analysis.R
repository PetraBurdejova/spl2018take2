####Histogram of total crime commited
ggplot(data=agg, aes(agg$total.crime)) + 
  geom_histogram(breaks=seq(0, 1200, by = 50), col="black", fill="blue", alpha = .5) + 
  labs(title="Histogram Total Crime") +
  labs(x="Total Crime", y="Count") +
  xlim(c(0,1200)) 

###Histogram Assaults
ggplot(data=agg, aes(agg$assault)) + 
  geom_histogram(breaks=seq(0, 650, by = 25), col="black", fill="blue", alpha = .5) + 
  labs(title="Histogram Assaults") +
  labs(x="Assaults", y="Count") +
  xlim(c(0,650)) 

###Histogram Auto Thefts
ggplot(data=agg, aes(agg$auto.theft)) + 
  geom_histogram(breaks=seq(0, 280, by = 20), col="black", fill="blue", alpha = .5) + 
  labs(title="Histogram Auto Thefts") +
  labs(x="Auto Thefts", y="Count") +
  xlim(c(0,300)) 



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

###Crime types by hour
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

###Use kmeans clustering to group neighbourhoods
library("cluster")
agg.kmeans <- join(neigh.codes, agg, by = "Hood_ID")
agg.kmeans <- agg.kmeans[,-c(1, 2)]

#Scale data
m <- apply(agg.kmeans, 2, mean)
s <- apply(agg.kmeans, 2, sd)
z <- scale(agg.kmeans, m, s)


wss <- (nrow(z)-1) * sum(apply(z, 2, var))
for (i in 2:20) wss[i] <- sum(kmeans(z, centers=i)$withiness)
plot(1:20, wss, type='b', xlab='Number of Clusters', ylab='Within groups sum of squares')

kc <- kmeans(z, 2)
kc

z1 <- data.frame(z, kc$cluster)
clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis')

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
  geom_polygon(aes(fill= density)) +    # draw polygons and add fill with population variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Density of Neighbourhoods")
print(g.pop.density)

#Plot neighbourhoods by robberies
g.robberies <- ggplot(data=toronto.geo, aes(x=long, y=lat, group=group))  + 
  geom_polygon(aes(fill= robbery)) +    # draw polygons and add fill with population variable
  geom_path(color="grey" ) +  # draw boundaries of neighbourhoods
  coord_equal() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")+
  labs(title="Roberries by Neighbourhood")
print(g.robberies)


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
