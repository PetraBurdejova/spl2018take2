rm(list=ls())



if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE); library("tidyverse")
if(!require("xts")) install.packages("xts", dependencies = TRUE); library("xts")
if(!require("zoo")) install.packages("zoo", dependencies = TRUE); library("zoo")
if(!require("chron")) install.packages("chron", dependencies = TRUE); library("chron")
if(!require("forecast")) install.packages("forecast", dependencies = TRUE); library("forecast")


setwd("C:/Users/Felix/Desktop/Uni/Berlin/2. Semester/SPL/spl2018")



#a <- read_csv("MCI_2014_to_2017.csv")
a <- read_csv("Robbery_2014_to_2017.csv")
b <- read_csv("2016_neighbourhood_profiles.csv")

c <- t(b)


d <- c()
c <- a %>% separate(Neighbourhood, "Neighbourhood", sep = "\\(", remove = TRUE, convert = FALSE)

df1 <- b %>%
  select(-`Data Source`) %>%
  gather(Neighbourhood, Value, -Characteristic, -Category, -Topic)

##funktioniert bis hier 

df2 <- df1 %>%
  group_by(Characteristic) %>%
  mutate(id = 1:n()) %>%
  spread(Characteristic, Value)






library(ggplot2)
install.packages("ggmap")
library(ggmap)
library(viridis)


toronto_map <- get_map(location = "toronto", maptype = "satellite", zoom = 12)

ggmap(toronto_map, extent = "device") + geom_point(aes(x = X, y = Y), colour = "red", 
                                                 alpha = 0.1, size = 2, data = a)

ggmap(toronto_map) +
  stat_density2d(data = a, aes(x = X, y = Y, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +
  scale_fill_viridis(option = 'inferno')
#(from: http://www.sharpsightlabs.com/blog/how-to-create-a-crime-heatmap-in-r/)



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