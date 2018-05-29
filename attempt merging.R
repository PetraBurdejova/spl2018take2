ndata <- read.csv("2016_neighbourhood_profiles.csv")
aggregated <- read.csv("aggregated.csv")
ndatasmall <- ndata[,-c(1:3,5)]
ndatasmall <- t(ndatasmall)
