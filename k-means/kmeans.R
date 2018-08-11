source("Merging.R")
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


# Define cluster variabes to be plotted on heat map
clust.var <- names(agg.2016[, grepl(".clust", colnames(agg.2016)), with = FALSE])

# plot cluster variables on a heatmap
lapply(clust.var, function(x) {HeatMapClust(toronto.geo, x)})
