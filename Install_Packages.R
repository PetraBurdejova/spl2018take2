###create a vector with packages names
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
              "stringr",
              "car",
              "lmtest", 
              "nortest",
              "corrplot",
              "stargazer",
              "sf",
              "xtable")

#create a function that loads the package if it is needed
usePackage <-function(p){
  if (!is.element(p, installed.packages()[,1])){
    print(paste('Package:',p,'Not found, Installing Now...'))
    install.packages(p, dep = TRUE)}
  print(paste('Loading Package :',p))
  require(p, character.only = TRUE)  
}
#load the packages
for(i in packages) {(usePackage(i))}
