# read the map of toronto 
toronto_map <- st_read("../spl2018take2/data/Shapefiles/Neighbourhoods_Toronto", layer = "NEIGHBORHOODS_WGS84")

# read the "green area" (parks, forests, ...) of toronto
green_shp <- st_read("../spl2018take2/data/Shapefiles/Green_Space_Toronto", layer = "CITY_GREEN_SPACE_WGS84")


# intersection + checking if it works
int <- st_intersection(toronto_map, green_shp)
plot(toronto_map$geometry, axes = TRUE)
plot(green_shp$geometry, add = TRUE, col = 'green')
plot(int$geometry, add = TRUE, col = 'red')


# add in areas in m^2
addArea <- int %>% 
  mutate(area = st_area(.) %>% as.numeric())

# and area in km^2
green_area <- addArea %>% group_by(AREA_S_CD) %>% summarise(Aream2 = sum(area)) %>% mutate(Area = Aream2/1000000)
area <- read_csv("../spl2018take2/data/toronto_area.csv")


# calculate the ratio of green area in toronto to whole area
greenarea <- c()
greenarea$ID <- area$`Neighbourhood Id`
greenarea$ratio <- (green_area$Area / area$`Total Area`) * 100

agg.2016$greenarea <- greenarea$ratio

# check if there is a chance that the ratio of parks and forests of a neighbourhood has a significant influence
plot(agg.2016$assault, agg.2016$greenarea) #  does not look like a dependence 

summary(greenmodel <- lm(robbery~greenarea, data = agg.2016))
summary(greenmodel1 <- lm(assault~greenarea, data = agg.2016))
#  -> not even close to be significant

