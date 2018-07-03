


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
               "plyr",
               "spdep",
               "cluster",
               "scales", 
               "readr",
               "stingr")

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE); library("tidyverse")
if(!require("reshape2")) install.packages("reshape2", dependencies = TRUE); library("reshape2")
if(!require("ggmap")) install.packages("ggmap", dependencies = TRUE); library("ggmap")
if(!require("stingr")) install.packages("stingr", dependencies = TRUE); library("stingr")

# packages <-
# 
# 
#   InstalledPackage <- function(package) 
#   {
#     available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
#     missing <- package[!available]
#     if (length(missing) > 0) return(FALSE)
#     return(TRUE)
#   }
# 
# CRANChoosen <- function()
# {
#   return(getOption("repos")["CRAN"] != "@CRAN@")
# }
# 
# UsePackage <- function(package, defaultCRANmirror = "http://cran.at.r-project.org") 
# {
#   if(!InstalledPackage(package))
#   {
#     if(!CRANChoosen())
#     {       
#       chooseCRANmirror()
#       if(!CRANChoosen())
#       {
#         options(repos = c(CRAN = defaultCRANmirror))
#       }
#     }
#     
#     suppressMessages(suppressWarnings(install.packages(package)))
#     if(!InstalledPackage(package)) return(FALSE)
#   }
#   return(TRUE)
# }
# UsePackage("tidyverse")
