rm(list=ls())

# set working directory

  setwd("C:/Users/Felix/Desktop/Uni/Berlin/2. Semester/SPL/spl2018take2")


# install all packages if needed

  source("Install_packages.R")

# adding and manipulating datasets
  
  source("Merging.R") 


# Initial Exploratory Analysis - contains functions for graphs and kmeans

  source("Initial_exploratory_analysis.R")

# Construct F-test by hand to show if coefficients are jointly significant if needed

  source("F-Test.R")

# calculating the ratio of green parks etc. per neighbourhood 

  source("Green_area.R")


# regression analysis

  source("automated_regression.R")


# further regression analysis

  source("advanced_regressions.R")



