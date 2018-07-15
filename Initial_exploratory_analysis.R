#histogram function- create cunction to quickly print ggplot histograms
hist_func <- function(x, y, bin.width) {
  Max <- max(x[[y]])
  print(ggplot(data= x, aes_string(y)) + #set data parameter and aesthetic variable
        geom_histogram(breaks = seq(0, Max, by = bin.width), col="black", fill="blue", alpha = .5) + #Set bin width 
        labs(title= paste("Histogram of", y, sep = " ", collapse = NULL)) + #Add title
        labs(x= y, y= "Count") + #Add x and y labels
        xlim(c(0, Max)))  #Set min and max values on x label
  }

####Histogram of total crime commited
hist_func(agg.crime, "total.crime", 50) #create histogram of total crime, bin width of 50
ggsave("plots_and_images/hist_total_crime.png", width=10, height=5, dpi=150)

###Histogram Assaults
hist_func(agg.crime, "assault", 25) #create histogram of assaults, bin width of 25
ggsave("plots_and_images/hist_assaults.png", width=10, height=5, dpi=150)

###Histogram Auto Thefts
hist_func(agg.crime, "auto.theft", 20) #create histogram of auto thefts, bin width of 20
ggsave("plots_and_images/hist_auto_thefts.png", width=10, height=5, dpi=150)

###Histogram Break and Enters
hist_func(agg.crime, "break.and.enter", 20) #create histogram of break and enters, bin width of 20
ggsave("plots_and_images/hist_break_n_enters.png", width=10, height=5, dpi=150)

###Histogram robberies
hist_func(agg.crime, "robbery", 10) #create histogram of robberies, bin width of 10
ggsave("plots_and_images/hist_robberies.png", width=10, height=5, dpi=150)

###Histogram Thefts
hist_func(agg.crime, "theft.over", 5) #create histogram of thefts, bin width of 5
ggsave("plots_and_images/hist_thefts.png", width=10, height=5, dpi=150)

###Histogram Drug Arrests
hist_func(agg.crime, "drug.arrests", 10) #create histogram of drug arrests, bin width of 10
ggsave("plots_and_images/hist_drug_arrests.png", width=10, height=5, dpi=150)

###Group crimes by MCI
mci.group <- group_by(crime.dt, MCI)
crime.by.mci <- dplyr::summarise(mci.group, n=n()) #count of events by MCI
crime.by.mci <- crime.by.mci[order(crime.by.mci$n, decreasing = TRUE),] #order crime by type from most to least 

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
hour.group <- group_by(crime.dt, occurrencehour)
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
crime.type.by.hour <- group_by(crime.dt, occurrencehour, MCI)
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
location.group <- group_by(crime.dt, Neighbourhood)
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
offence.location.group <- group_by(crime.dt, Neighbourhood, MCI) #group crime by neighbourhod and offence
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
crime.count <- group_by(crime.dt, occurrencemonth, MCI) %>% dplyr::summarise(Total = n())
crime.count$occurrencemonth <- ordered(crime.count$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

plot(ggplot(subset(crime.count, MCI %in% c("Theft Over" , "Robbery", "Break and Enter", "Auto Theft")), 
            aes(occurrencemonth, MCI, fill = Total)) +
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
set.seed(123)

# Test different numbers of clusters ####
# Define a vector with candidate settings for k
k.settings = 2:15

obj.values = vector(mode="numeric", length = length(k.settings))

my_kMeans <- function(data,k) {  
  clu.sol <- kmeans(data, centers=k) #function to test different number of clusters
  return(clu.sol$tot.withinss)
}

obj_values <- function(x ,title) {
  
  obj.values <- sapply(k.settings, my_kMeans, data = x) #apply kmeans function accross vect of k setings values
  
  
  k.clust <- data.frame(k.settings, obj.values)
  print(plot(ggplot(k.clust, aes(k.settings,obj.values))+
               geom_line(color = "red") + 
               geom_point(color="red")  + 
               xlab("k") + ylab("Total within-cluster SS") + 
               ggtitle(paste("Elbow curve for k selection using", title, sep = " ", collapse = NULL))))
  
}

#Define function to label each neighbourhood with cluster label
clust_func <- function(x, no.of.clust, title) {
  kc <- kmeans(x, no.of.clust) #use optimal number of clusters
  z1 <- data.frame(x, kc$cluster) #create data frame with cluster number for each neighbourhood
  print(clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main=paste('k-Means Cluster Analysis', title, sep = " ", collapse = NULL))) #cluster plot
  y <- as.factor(z1$kc.cluster)
  return(y)
}

crime.vars <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests")
agg.kmeans.crime <- agg.2016[, crime.vars, with = FALSE]

obj_values(agg.kmeans.crime, "crime statistics")

ggsave("plots_and_images/elbow_curve_crime_stats.png", width=10, height=5, dpi=150)

agg.2016$crime.clust <- clust_func(agg.kmeans.crime, 8, "Crime Statistics") #join crime clusters to agg.2016 data frame

####Use kmeans to group variables based on neighbourhood characteristics
neigh.vars <- c("male.youth", "youth", "population.2016", "density", "lone.parent.families")
agg.kmeans.neigh.vars <- agg.2016[, neigh.vars, with = FALSE]

obj_values(agg.kmeans.neigh.vars, "pop characteristics") #find optimal number of clusters

ggsave("plots_and_images/elbow_curve_pop_char.png", width=10, height=5, dpi=150)

agg.2016$neigh.clust <- clust_func(agg.kmeans.neigh.vars, 8, "Pop Characteristics")

####Use kmeans to group variables based on neighbourhood income characteristics
inc.vars <- c("low.income", "middle.income", "high.income", "avg.income", "people.ei", "median.income",
              "no.hholds.bottom.20per", "low.income.pop")
agg.kmeans.inc <- agg.2016[, inc.vars, with = FALSE]

obj_values(agg.kmeans.inc, "income characteristics") #find optimal number of clusters

ggsave("plots_and_images/elbow_curve_income_char.png", width=10, height=5, dpi=150)

agg.2016$inc.clust <- clust_func(agg.kmeans.inc, 7, "Income Characteristics")

####Use kmeans to group variables based on neighbourhood ethnicity characteristics
ethnicity.vars <- c("non.citizens", "immigrants", "refugees", "vis.minorities")
agg.kmeans.ethnic <- agg.2016[, ethnicity.vars, with = FALSE]

obj_values(agg.kmeans.ethnic, "ethnic characteristics")

ggsave("plots_and_images/elbow_curve_income_char.png", width=10, height=5, dpi=150)

agg.2016$ethnic.clust <- clust_func(agg.kmeans.ethnic, 4, "Ethnic Characteristics")

####Use kmeans to group variables based on neighbourhood housing characteristics
house.vars <- c("houses", "hhlds.mjr.rprs")
agg.kmeans.hhlds <- agg.2016[, house.vars, with = FALSE]

obj_values(agg.kmeans.hhlds, "house characteristics")

ggsave("plots_and_images/elbow_curve_house_char.png", width=10, height=5, dpi=150)

agg.2016$houses.clust <- clust_func(agg.kmeans.hhlds, 6, "House Characteristics")

####Use kmeans to group variables based on neighbourhood education characteristics
education.vars <- c("less.than.high.school", "high.school.cert", "post.sec.or.above")
agg.kmeans.edu <- agg.2016[, education.vars, with = FALSE]

obj_values(agg.kmeans.edu, "education variables")

ggsave("plots_and_images/elbow_curve_edu_char.png", width=10, height=5, dpi=150)

agg.2016$edu.clust <- clust_func(agg.kmeans.edu, 6, "Education")

#Heatmap of toronto by population 
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


#Define function to generate heat maps (input dataframe and desired column)
heat_map <- function(data, x) {
  a <- ggplot(data= data, aes(x=long, y=lat, group=group))  + #Choose dataframe and plot neighbourhood lines
    geom_polygon(aes_string(fill= x)) +    # draw polygons and add fill with chosen variable
    geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() + 
    scale_fill_gradient(low = "#7ff4f0", high = "#000c8c",  #set colour scale
                        space = "Lab", na.value = "grey50",
                        guide = "colourbar")+
    labs(title= x)  #set title
  print(a) # render the map
}

#Define function to generate heat maps for cluster variables (input dataframe and desired cluster)
heat_map_clust <- function(data, x) {
  a <- ggplot(data= data, aes(x=long, y=lat, group=group))  + 
    geom_polygon(aes_string(fill= x)) +    # draw polygons and add fill with variable
    geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() +
    geom_tile() + #plot fill as geom tiles
    scale_fill_brewer(palette="Blues") + #choose colour palette
    labs(title= x)  #set title
  print(a) # render the map
}

#Define function to generate heat maps with an upper limit on scale (input dataframe and desired cluster)
#for variables with an outlier that throws off the colour scale
heat_map_limit <- function(data, x, lower, upper) {
  a <- ggplot(data=data, aes(x=long, y=lat, group=group))  + 
    geom_polygon(aes_string(fill= x, colour = shQuote(""))) +    # draw polygons and add fill with density variable
    geom_path(color="light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() + 
    scale_fill_gradient(low = "#7ff4f0", high = "#000c8c",  #Set colour scale
                        space = "Lab", 
                        na.value = "#000647", 
                        limits = c(lower, upper), #Nas are grey, set upper and lower limits of scale
                        guide = "colourbar") + #Add colour scale on side
    scale_colour_manual(values = NA) +              
    guides(colour=guide_legend(paste(">", upper, sep = " ", collapse = NULL), override.aes = list(fill="#000647"))) + #label guide
    labs(title= x) #Add title
  print(a)
}

#Define crime variables that will be plotted as a heatmap
crime.var <- c("total.crime", "assault", "robbery", "break.and.enter",
               "drug.arrests", "theft.over", "auto.theft")

for (i in crime.var) {
  heat_map(toronto.geo, i)
}

heat_map_limit(toronto.geo, "auto.theft", 0, 100)
heat_map_limit(toronto.geo, "drug.arrests", 0, 100)
heat_map_limit(toronto.geo, "robbery", 0, 80)
heat_map_limit(toronto.geo, "assault", 0, 500)
heat_map_limit(toronto.geo, "total.crime", 0, 500)

#Define crime rates that will be plotted as a heatmap
crime.var.per.tenthsnd <- names(agg.2016[, grepl("per.tenthsnd", colnames(agg.2016)), with = FALSE])

for (i in crime.var.per.tenthsnd) {
  heat_map(toronto.geo, i)
}

heat_map_limit(toronto.geo, "auto.theft.per.tenthsnd", 0, 50)
heat_map_limit(toronto.geo, "drug.arrests.per.tenthsnd", 0, 50)

#define variables from census data to be plotted as a heatmap
heat.map.var <- c( "population.2016", "male.youth", "median.income", "hholds.bottom.20per.per",
                   "low.income.pop.per", "immigrants.per", "vis.minorities.per",
                   "renters.per", "hhlds.mjr.rprs.per", "unaffordable.housing.per",
                   "unemployment.rate") #Define variables to be plotted as heat map

for (i in heat.map.var) {
  heat_map(toronto.geo, i)
}

heat_map_limit(toronto.geo, "density", 0, 15000)
heat_map_limit(toronto.geo, "avg.income", 25000, 100000)


#Define cluster variabes to be plotted on heat map
clust.var <- names(agg.2016[, grepl(".clust", colnames(agg.2016)), with = FALSE])

for (i in clust.var) {
  heat_map_clust(toronto.geo, i)
}
