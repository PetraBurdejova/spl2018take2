
toronto_map <- st_read(".", layer = "NEIGHBORHOODS_WGS84")

green_shp <- st_read(".", layer = "CITY_GREEN_SPACE_WGS84")


# intersection + checking if it works
int <- st_intersection(toronto_map, green_shp)
plot(toronto_map$geometry, axes = TRUE)
plot(green_shp$geometry, add = TRUE, col = 'green')
plot(int$geometry, add = TRUE, col = 'red')


# add in areas in m2
addArea <- int %>% 
  mutate(area = st_area(.) %>% as.numeric())

# and area in km^2
green_area <- addArea %>% group_by(AREA_S_CD) %>% summarise(Aream2 = sum(area)) %>% mutate(Area = Aream2/1000000)
area <- read_csv("toronto_area.csv")

greenarea <- c()
greenarea$ID <- area$`Neighbourhood Id`
greenarea$ratio <- (green_area$Area / area$`Total Area`) * 100

agg.2016$greenarea <- greenarea$ratio
regresso <- lm(Assault ~ greenarea, data = agg.2016)


# 
# knn1 <- knn( coordinates(crime.sp), coordinates(city_center), k=1)

toronto_map <- get_map(location = "toronto", maptype = "satellite", zoom = 12)

#defining neighbours
shp <- readOGR(".", "NEIGHBORHOODS_WGS84")
###based on queen approach
neigh <- poly2nb(shp, queen = TRUE)
W<-nb2listw(neigh, style="W", zero.policy=TRUE)
W
plot(W, coordinates(shp))

##based on distance 
coords<-coordinates(shp)
W_dist<-dnearneigh(coords,0,2.5,longlat = TRUE)
 ### -> check out which ones better -> Moran'S I test 

#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
