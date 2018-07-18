# 
# 
# 
# if(!require("pacman")) install.packages("pacman", dependencies = TRUE); library("pacman")
# 
# 
# 
# pacman::p_load("tidyverse", 
#                "xts", 
#                "zoo", 
#                "chron", 
#                "reshape2", 
#                "ggplot2", 
#                "ggmap", 
#                "viridis", 
#                "forecast",
#                "sp",
#                "rgdal",
#                "data.table",
#                "maptools",
#                "sf",
#                "lwgeom", 
#                "nabor",
#                "plyr",
#                "spdep",
#                "cluster",
#              "scales",
#                "caret",
#              "scales", 
#                "readr",
#                "stingr")

packages <- c("tidyverse", 
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
              "plyr",
              "spdep",
              "cluster",
              "scales",
              "caret",
              "readr",
              "stringr")

usePackage <-function(p){
  if (!is.element(p, installed.packages()[,1])){
    print(paste('Package:',p,'Not found, Installing Now...'))
    install.packages(p, dep = TRUE)}
  print(paste('Loading Package :',p))
  require(p, character.only = TRUE)  
}

for(i in packages) {(usePackage(i))}
