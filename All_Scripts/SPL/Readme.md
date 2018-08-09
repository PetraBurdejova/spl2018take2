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

####set working directory####
    setwd("C:/Users/Felix/Desktop/Uni/Berlin/2. Semester/SPL/spl2018take2")


####install all packages if needed####
    source("Install_packages.R")


####manipulate dataset####
    source("Merging.R") # code I deleted in SPL_X1 can be found in deleted_code


#####Initial Exploratory Analysis###

    source("Initial_exploratory_analysis.R")

#####Construct F-test by hand to show if coefficients are jointly significant if needed####

    source("F-Test.R")

####calculating the ratio of green parks etc. per neighbourhood ####

    source("Green_area.R")

###regression analysis

    source("automated_regression.R")

##further regression analysis

    source("advanced_regressions.R")


