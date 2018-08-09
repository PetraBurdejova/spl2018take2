rm(list=ls())

#### set working directory

  setwd("C:/Users/Felix/Desktop/Uni/Berlin/2. Semester/SPL/spl2018take2")


#### install all packages if needed

<<<<<<< HEAD
  source("Install_packages.R")
=======
####manipulate dataset####
    source("Merging.R") 
>>>>>>> ec03062cf348b098f8f9819d74f9df6b0f14d047


#### adding and manipulating datasets####
  
  source("Merging.R") 


#### Initial Exploratory Analysis - contains functions for graphs and kmeans###

  source("Initial_exploratory_analysis.R")

##### Construct F-test by hand to show if coefficients are jointly significant if needed####

  source("F-Test.R")

#### calculating the ratio of green parks etc. per neighbourhood ####

  source("Green_area.R")


### regression analysis

  source("automated_regression.R")


## further regression analysis

  source("advanced_regressions.R")



