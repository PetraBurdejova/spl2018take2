





toronto_map <- get_map(location = "toronto", maptype = "satellite", zoom = 12)

ggmap(toronto_map, extent = "device") + geom_point(aes(x = X, y = Y), colour = "red", 
                                                 alpha = 0.1, size = 2, data = a)

ggmap(toronto_map) +
  stat_density2d(data = a, aes(x = X, y = Y, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +
  scale_fill_viridis(option = 'inferno')
#(from: http://www.sharpsightlabs.com/blog/how-to-create-a-crime-heatmap-in-r/)


shp <- readOGR(".", "NEIGHBORHOODS_WGS84")

# with polygon 
# ggplot(map.df, aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=Count))+
#   geom_path()+ 
#   scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
#   coord_map()
# 



# toronto_map_g_str <- get_map(location = "toronto", zoom = 10)
# 
# ggmap(toronto_map_g_str, extent = "device") + geom_density2d(data = a, 
#                                                            aes(x = X, y = Y), size = 0.3) + stat_density2d(data = a, 
#                                                                                                                aes(x = X, y = Y, fill = ..level.., alpha = ..level..), size = 0.01, 
#                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
#   scale_alpha(range = c(0, 0.3), guide = FALSE)


#https://www.r-bloggers.com/visualising-thefts-using-heatmaps-in-ggplot2/

# #https://rpubs.com/jimu_xw/crime_visualization
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/02-Static-Maps.nb.html # for dynamic ones 