


if(!require("pacman")) install.packages("pacman", dependencies = TRUE); library("pacman")



pacman::p_load("tidyverse", 
               "xts", 
               "zoo", 
               "chron", 
               "reshape2", 
               "ggplot2", 
               "ggmap", 
               "viridis", 
               "forecast",
               "sp",
               "rgdal",
               "data.table",
               "maptools",
               "sf",
               "lwgeom", 
               "nabor",
               "plyr")
