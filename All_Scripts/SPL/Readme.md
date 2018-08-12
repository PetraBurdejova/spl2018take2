[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL - Main Script** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Main Script


Description: Script that sources all other scripts that are used

Keywords: regression, organization

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```


### R Code
```r
rm(list=ls())

# set working directory

  setwd("../spl2018take2")


# install all packages if needed

  source("../spl2018take2/All_scripts/Install_packages/Install_Packages.R")

# adding and manipulating datasets
  
  source("../spl2018take2/All_scripts/Merging/Merging.R") 


# Initial Exploratory Analysis - contains functions for graphs and kmeans

  source("../spl2018take2/All_scripts/Initial_exploratory_analysis/Initial_exploratory_analysis.R")

# Construct F-test by hand to show if coefficients are jointly significant if needed

  source("../spl2018take2/All_scripts/F-Test/F-Test.R")

# calculating the ratio of green parks etc. per neighbourhood 

  source("../spl2018take2/All_scripts/Green_area/Green_area.R")

# checking original vs transformed data
  
  source("../spl2018take2/All_scripts/transformed_vs_original_data/transformed_vs_original_data.R")
  
# regression analysis

  source("../spl2018take2/All_scripts/automated_regression/automated_regression.R")


# further regression analysis

  source("../spl2018take2/All_scripts/advanced_regressions/advanced_regressions.R")



