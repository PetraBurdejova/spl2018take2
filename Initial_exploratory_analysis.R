# histogram function
HistFunc <- function(x, y, bin.width) {
  # function to quickly print ggplot histograms and specify bin widths
  #
  # Args:
  #
  #   x: data frame or table to be used
  #   y: column with data to be plotted as histogram
  #   bin.width: bin width to split continuous variable into intervals
  #
  #   Returns: a histogram of the specified variable
  Max <- max(x[[y]]) # max value of variable
  print(ggplot(data = x, aes_string(y)) + # select data frameto be used and aesthetic variable is variable to be plotted
        geom_histogram(breaks = seq(0, Max, by = bin.width), col="black", fill ="blue", alpha = .5) + # Set max value onn scale to be max value of variable and bin width 
        labs(title = paste("Histogram of", y, sep = " ", collapse = NULL)) + # Add title
        labs(x = y, y = "Count") + #Add x and y labels
        xlim(c(0, Max)))  #Set min and max values on x label
  
}

# Histogram of total crime commited
HistFunc(agg.crime, "total.crime", 50) # create histogram of total crime, bin width of 50

# Histogram Assaults
HistFunc(agg.crime, "assault", 25) # create histogram of assaults, bin width of 25

# Histogram Auto Thefts
HistFunc(agg.crime, "auto.theft", 20) # create histogram of auto thefts, bin width of 20

# Histogram Break and Enters
HistFunc(agg.crime, "break.and.enter", 20) # create histogram of break and enters, bin width of 20

# Histogram robberies
HistFunc(agg.crime, "robbery", 10) # create histogram of robberies, bin width of 10

# Histogram Thefts
HistFunc(agg.crime, "theft.over", 5) # create histogram of thefts, bin width of 5

# Histogram Drug Arrests
HistFunc(agg.crime, "drug.arrests", 10) # create histogram of drug arrests, bin width of 10

# Group crimes by MCI
mci.group <- group_by(crime.dt, MCI)
crime.by.mci <- dplyr::summarise(mci.group, n = n()) # count of events by MCI
crime.by.mci <- crime.by.mci[order(crime.by.mci$n, decreasing = TRUE), ] # order crime by type from most to least 

# plot crimes by type, order by most common to least common
 plot(ggplot(aes(x = reorder(MCI, n), y = n), data = crime.by.mci) + # order MCI's by number of occurrences, use crime by MCI data
  geom_bar(stat = 'identity', width = 0.5) + # bar graph
  geom_text(aes(label = n), stat = 'identity', data = crime.by.mci, hjust = -0.1, size = 3.5) +
  coord_flip() + # flip x and y coordinates
  xlab('Major Crime Indicators') + # x-axis label
  ylab('Number of Occurrences') + # y-axis label 
  ggtitle('Major Crime Indicators Toronto 2016') + # title
  theme_bw() + 
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))


# Group crimes by time of day
hour.group <- group_by(crime.dt, occurrencehour)
crime.hour <- dplyr::summarise(hour.group, n = n()) # count of crimes by hour

# plot crime by time of day
plot(ggplot(aes(x=occurrencehour, y = n), data = crime.hour) + geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1) + 
  geom_point(size = 0.5) + 
  ggtitle('Total Crimes by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))


# Crimes by MCI by hour
crime.type.by.hour <- group_by(crime.dt, occurrencehour, MCI) # group data by hour and MCI
hour.crime <- dplyr::summarise(crime.type.by.hour, n = n()) # count of crime types by hour

plot(ggplot(aes(x = occurrencehour, y = n, color = MCI, linetype = MCI), data = hour.crime) + # plot lines by MCI 
  geom_line(size = 1.5) + 
  ggtitle('Crime Types by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))


# neighbourhoods with most crime
location.group <- group_by(crime.dt, Neighbourhood) # group crime by neighbourhood
crime.by.location <- dplyr::summarise(location.group, n = n()) # sum MCIs by neighbourhood
crime.by.location <- crime.by.location[order(crime.by.location$n, decreasing = TRUE), ] # order neighbourhoods by crime
crime.by.location.top20 <- head(crime.by.location, 20) # top 20 neighbourhoods by crime

plot(ggplot(aes(x = reorder(Neighbourhood, n), y = n), data = crime.by.location.top20) +  # plot top 20 neighbourhoods with most crime
  geom_bar(stat = 'identity', width = 0.6) +
  geom_text(aes(label = n), stat = 'identity', data = crime.by.location.top20, hjust = -0.1, size = 3) +
  coord_flip() +
  xlab('Neighbourhoods') +
  ylab('Number of Occurrences') +
  ggtitle('Neighbourhoods with Most Crimes - Top 20') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))


# offence types by neighbourhood
offence.location.group <- group_by(crime.dt, Neighbourhood, MCI) # group crime by neighbourhod and offence
offence.type.by.location <- dplyr::summarise(offence.location.group, n = n()) # get counts of crimes by neighbourhood
offence.type.by.location <- offence.type.by.location[order(offence.type.by.location$n, decreasing = TRUE), ]
offence.type.by.location.top20 <- head(offence.type.by.location, 20)

plot(ggplot(aes(x = Neighbourhood, y=n, fill = MCI), data = offence.type.by.location.top20) + # plot top 20 neighbourhoods with most crime by type of crime
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  xlab('Neighbourhood') +
  ylab('Number of Occurrence') +
  ggtitle('Offence Type vs. Neighbourhood Toronto 2016') + theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)))


# Crime count by month
crime.count <- group_by(crime.dt, occurrencemonth, MCI) %>% dplyr::summarise(Total = n())
crime.count$occurrencemonth <- ordered(crime.count$occurrencemonth, 
                                       levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

plot(ggplot(subset(crime.count, MCI %in% 
                     c("Theft Over" , "Robbery", "Break and Enter", "Auto Theft")), # plot crimes by month
            aes(occurrencemonth, MCI, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_gradient(
    low = "#7ff4f0",
    high = "#000c8c"
  )  +
  geom_text(aes(label = Total), color = 'black') +
  ggtitle("Major Crime Indicators by Month 2016") +
  xlab('Month') +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold")))


# Use kmeans clustering to group neighbourhoods based on crime statistics
library("cluster")
set.seed(123)

# Test different numbers of clusters ####
# Define a vector with candidate settings for k
k.settings = 2:15

# create emoty vector to store objectie values for each value of k
obj.values = vector(mode="numeric", length = length(k.settings))

MyKmeans <- function(data,k) {  
  # function to test different number of clusters
  #
  # Args:
  #   data: data to be clustered
  #   k: number of clusters
  #
  # Result: total within sum of squares for k number of clusters 
  clu.sol <- kmeans(data, centers=k) 
  return(clu.sol$tot.withinss)
}


ObjValues <- function(x ,title) {
  # function to apply kmeans function over varying number of clusters
  #
  # Args:
  #   x: data to be clustered
  #   title: title of plot
  #
  # Return: Elbow curve plot with number of clusters vs total within sum of squares
  obj.values <- sapply(k.settings, MyKmeans, data = x) # apply kmeans function accross vect of k settings values
  
  k.clust <- data.frame(k.settings, obj.values) # create an object with various k values and corresponding total within sum of squares
  
  print(plot(ggplot(k.clust, aes(k.settings,obj.values))+ # plot elbow curve
               geom_line(color = "red") + 
               geom_point(color="red")  + 
               xlab("k") + ylab("Total within-cluster SS") + 
               ggtitle(paste("Elbow curve for k selection using", title, sep = " ", collapse = NULL))))
  }


ClustFunc <- function(x, no.of.clust, title) {
  # function to label each neighbourhood with its cluster number
  #
  # Args:
  #   x: data to be joined to cluster numbers
  #   no.of.clust: optimal number of clusters to be used, as determined from elbow curve
  #   title: title of cluster plot
  #
  # Return: cluster plot using optimal number of clusters as determined from elbow curve
  kc <- kmeans(x, no.of.clust) # use optimal number of clusters
  z1 <- data.frame(x, kc$cluster) # create data frame with cluster number for each neighbourhood
  print(clusplot(z1, kc$cluster, color = TRUE, shade = F, labels = 0, lines = 0, main = paste('k-Means Cluster Analysis', title, sep = " ", collapse = NULL))) # cluster plot
  y <- as.factor(z1$kc.cluster) # set cluster number as a factor and not numeric
  return(y)
}

# define crime variables
crime.vars <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests")
agg.kmeans.crime <- agg.2016[, crime.vars, with = FALSE] # subset agg.2016 by crime variables

ObjValues(agg.kmeans.crime, "crime statistics") # find elbow curve for crime statistics

agg.2016$crime.clust <- ClustFunc(agg.kmeans.crime, 8, "Crime Statistics") # join crime clusters to agg.2016 data frame

# Use kmeans to group variables based on population characteristics
# define population variables
neigh.vars <- c("male.youth", "youth", "population.2016", "density", "lone.parent.families")
agg.kmeans.neigh.vars <- agg.2016[, neigh.vars, with = FALSE] # subset agg.2016 by neighbourhood variables

ObjValues(agg.kmeans.neigh.vars, "pop characteristics") # find optimal number of clusters

agg.2016$neigh.clust <- ClustFunc(agg.kmeans.neigh.vars, 8, "Pop Characteristics") # join population clusters to agg.2016

# Use kmeans to group variables based on neighbourhood income characteristics
# define income variables
inc.vars <- c("low.income", "middle.income", "high.income", "avg.income", "people.ei", "median.income",
              "no.hholds.bottom.20per", "low.income.pop")
agg.kmeans.inc <- agg.2016[, inc.vars, with = FALSE] # subset agg.2016 by income variables

ObjValues(agg.kmeans.inc, "income characteristics") # find optimal number of clusters

agg.2016$inc.clust <- ClustFunc(agg.kmeans.inc, 7, "Income Characteristics") # join income clusters to agg.2016 

# Use kmeans to group variables based on neighbourhood ethnicity characteristics
# define ethnicity variables
ethnicity.vars <- c("non.citizens", "immigrants", "refugees", "vis.minorities")
agg.kmeans.ethnic <- agg.2016[, ethnicity.vars, with = FALSE] # subset agg.2016 by ethnicity variables

ObjValues(agg.kmeans.ethnic, "ethnic characteristics") # find optiaml number of clusters

agg.2016$ethnic.clust <- ClustFunc(agg.kmeans.ethnic, 4, "Ethnic Characteristics") # join ethnic clusters to agg.2016 

# Use kmeans to group variables based on neighbourhood housing characteristics
# define housing variables
house.vars <- c("houses", "hhlds.mjr.rprs")
agg.kmeans.hhlds <- agg.2016[, house.vars, with = FALSE] # subset agg.2016 by housing characteristics

ObjValues(agg.kmeans.hhlds, "house characteristics") # find optimal number fo clusters

agg.2016$houses.clust <- ClustFunc(agg.kmeans.hhlds, 6, "House Characteristics") # join housing clusters to agg.2016

# Use kmeans to group variables based on neighbourhood education characteristics
# define education variables
education.vars <- c("less.than.high.school", "high.school.cert", "post.sec.or.above")
agg.kmeans.edu <- agg.2016[, education.vars, with = FALSE] # subset agg.2016 by education variables

ObjValues(agg.kmeans.edu, "education variables") # find optimal number of clusters

agg.2016$edu.clust <- ClustFunc(agg.kmeans.edu, 6, "Education") # join education cluster to agg.2016

# Heatmap of toronto by population 
# Read the neighborhood shapefile data and plot
geo.data <- data.frame(agg.2016)
geo.data$Hood_ID <- str_pad(geo.data$Hood_ID, width = 3, side = 'left', pad = '0')

# the path to shape file

toronto <- readOGR(dsn = "Shapefiles/Neighbourhoods_Toronto" ,"NEIGHBORHOODS_WGS84")


# fortify and merge: muni.df is used in ggplot
toronto@data$id <- rownames(toronto@data)
toronto.geo <- fortify(toronto)
toronto.geo <- join(toronto.geo, toronto@data, by="id")
names(toronto.geo)[names(toronto.geo) == 'AREA_S_CD'] <- 'Hood_ID'

toronto.geo <- join(geo.data, toronto.geo, by = "Hood_ID") # join data from census to data from shapefile

# Define function to generate heat maps (input dataframe and desired column)
HeatMap <- function(data, x) {
  # A function to plot a geographic heatmap, using the shapefile data
  # 
  # Args:
  #   data: data frame to be used
  #   x: variable to be plotted on heat map, must be a string
  #
  # Return: A heat map of with chosen variable as the fill
  plot(ggplot(data = data, aes(x = long, y = lat, group=group))  + #Choose dataframe and plot neighbourhood lines
    geom_polygon(aes_string(fill = x)) +    # draw polygons and add fill with chosen variable
    geom_path(color = "light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() + 
    scale_fill_gradient(low = "#7ff4f0", high = "#000c8c",  #set colour scale
                        space = "Lab", na.value = "grey50",
                        guide = "colourbar")+
    labs(title = x))  #set title # render the map
}

# Define function to generate heat maps for cluster variables (input dataframe and desired cluster)
HeatMapClust <- function(data, x) {
  # Function to plot cluster variables from kmeans
  #
  # Args:
  #   data: data to be plotted
  #   x: fill variable, will be a factor variable in this case
  #
  # Returns: A geographical cluster plot of the cluster variables
  plot(ggplot(data = data, aes(x = long, y = lat, group = group))  + 
    geom_polygon(aes_string(fill = x)) +    # draw polygons and add fill with variable
    geom_path(color = "light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() +
    geom_tile() + #plot fill as geom tiles
    scale_fill_brewer(palette = "Blues") + # choose colour palette
    labs(title = x))  # set title # render the map
}

# Define function to generate heat maps with an upper limit on scale (input dataframe and desired cluster)
# for variables with an outlier that throws off the colour scale
HeatMapLimit <- function(data, x, lower, upper) {
  # Heat map with an upper limit on the scale
  #
  # Args:
  #   data: data to be used
  #   x: variable t be used as fill
  #   lower: lower cutoff of the scale
  #   upper: upper cutoff of the scale
  #
  # Returns: A heatmap with a lower and upper limit to reduce the distortion of outliers on the scale
  plot(ggplot(data = data, aes(x = long, y = lat, group = group))  + 
    geom_polygon(aes_string(fill = x, colour = shQuote(""))) +    # draw polygons and add fill with density variable
    geom_path(color = "light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() + 
    scale_fill_gradient(low = "#7ff4f0", high = "#000c8c",  # Set colour scale
                        space = "Lab", 
                        na.value = "#000647", # any value above upper limit or below lower limit will be considered as NAs 
                        limits = c(lower, upper), # Nas are darker colour than rest, set upper and lower limits of scale
                        guide = "colourbar") + # Add colour scale on side
    scale_colour_manual(values = NA) +              
    guides(colour=guide_legend(paste(">", upper, sep = " ", collapse = NULL), override.aes = list(fill="#000647"))) + # label guide
    labs(title = x)) # Add title
 
}

# Define crime variables that will be plotted as a heatmap
crime.var <- c("total.crime", "assault", "robbery", "break.and.enter",
               "drug.arrests", "theft.over", "auto.theft")

lapply(crime.var, function(x) {HeatMap(toronto.geo, x)}) # create heatmap of crime variables

HeatMapLimit(toronto.geo, "auto.theft", 0, 100)
HeatMapLimit(toronto.geo, "drug.arrests", 0, 100)
HeatMapLimit(toronto.geo, "robbery", 0, 80)
HeatMapLimit(toronto.geo, "assault", 0, 500)
HeatMapLimit(toronto.geo, "total.crime", 0, 500)

# Define crime rates per ten thousand that will be plotted as a heatmap
crime.var.per.tenthsnd <- names(agg.2016[, grepl("per.tenthsnd", colnames(agg.2016)), with = FALSE])

lapply(crime.var.per.tenthsnd, function(x) {HeatMap(toronto.geo, x)}) # create heatmap of crime rate variables

HeatMapLimit(toronto.geo, "auto.theft.per.tenthsnd", 0, 50)
HeatMapLimit(toronto.geo, "drug.arrests.per.tenthsnd", 0, 50)

# define variables from census data to be plotted as a heatmap
heat.map.var <- c( "population.2016", "male.youth", "median.income", "hholds.bottom.20per.per",
                   "low.income.pop.per", "immigrants.per", "vis.minorities.per",
                   "renters.per", "hhlds.mjr.rprs.per", "unaffordable.housing.per",
                   "unemployment.rate", "avg.income", "density") 

lapply(heat.map.var, function(x) {HeatMap(toronto.geo, x)}) # create heatmap of neighbourhood variables


HeatMapLimit(toronto.geo, "density", 0, 15000)
HeatMapLimit(toronto.geo, "avg.income", 25000, 100000)


# Define cluster variabes to be plotted on heat map
clust.var <- names(agg.2016[, grepl(".clust", colnames(agg.2016)), with = FALSE])

# plot cluster variables on a heatmap
lapply(clust.var, function(x) {HeatMapClust(toronto.geo, x)})
HeatMapClust(toronto.geo, "crime.clust")

HeatMap(toronto.geo, "unemployment.rate")
