


#heatmap of exact location

toronto_map <- get_map(location = "toronto", maptype = "satellite", zoom = 12)

a1 <- a %>% select(X, Y, occurrenceyear) %>% filter(occurrenceyear == 2016)

#not so nice
# ggmap(toronto_map, extent = "device") + geom_point(aes(x = X, y = Y), colour = "red",
#                                                  alpha = 0.1, size = 2, data = a1)

if (exists('.GeocodedInformation')) rm(.GeocodedInformation)

#nice 
ggmap(toronto_map) +
  stat_density2d(data = a1, aes(x = X, y = Y, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +
  scale_fill_viridis(option = 'inferno')
#(from: http://www.sharpsightlabs.com/blog/how-to-create-a-crime-heatmap-in-r/)


#heatmap per neighbourhood
#try to calculate the actual density


shp <- readOGR(".", "NEIGHBORHOODS_WGS84")


heatMap <-function(data,shape=NULL,col="blue",main="Sample HeatMap"){
  # Plots a Heat Map of a Polygons Data Frame.  This will 
  # demonstrate density within a finite set of polygons
  #
  # Args:
  #   data:   Spatial Points dataframe
  #   shape:  Polygons Data Frame 
  #
  #
  #   Notes:  This function requires the sp and RColorBrewer
  #           Packages
  #
  #   Beskow: 03/28/11   
  #
  is.installed <- function(mypkg) is.element(mypkg, 
                                             installed.packages()[,1])
  if (is.installed(mypkg="sp")==FALSE)  {
    stop("sp package is not installed")}
  if (is.installed(mypkg="RColorBrewer")==FALSE)  {
    stop("RColorBrewer package is not installed")}
  if (!class(data)=="SpatialPointsDataFrame")  {
    stop("data argument is not SpatialPointsDataFrame")}
  require(sp)
  require(RColorBrewer)
  freq_table<-data.frame(tabulate(over(as(data,"SpatialPoints"),
                                       as(shape,"SpatialPolygons")),nbins=length(shape)))
  names(freq_table)<-"counts"
  
  shape1<-spChFIDs(shape,as.character(1:length(shape)))
  row.names(as(shape1,"data.frame"))
  spdf<-SpatialPolygonsDataFrame(shape1, freq_table, match.ID = TRUE)
  
  rw.colors<-colorRampPalette(c("white",col))
  spplot(spdf,scales = list(draw = TRUE),
         col.regions=rw.colors(max(freq_table)), main=main)
}


library(sp)
library(RColorBrewer)


  
  
crime.sp<-a1    ##create a new object that we will coerce to a SpatialPointsDataFrame
coordinates(crime.sp)<-c("X","Y")       ##Assigning coordinates coerces this to a SpatialPointsDataFrame
proj4string(crime.sp)<-proj4string(shp)


heatMap(crime.sp,shp,col="red",main="Toronto Total Crimes per Neighbourhood")





#https://www.r-bloggers.com/visualising-thefts-using-heatmaps-in-ggplot2/

# #https://rpubs.com/jimu_xw/crime_visualization
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/02-Static-Maps.nb.html # for dynamic ones 