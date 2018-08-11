# Create function to get data from main dataset
getData <- function(x, characteristic, new.col.name = characteristic) {
  # A function to quickly get data from census dataset
  #
  # Args: 
  #
  #   x: data frame or data table from whichto collect data
  #   characteristic: column we are interested in getting from census data set
  #   new.col.name: change columnname of variable. Default is same as name from census data set
  #
  # Returns: A vector of values from census data set
  a <- subset(x, Characteristic == characteristic) # subset dataframe by characteristic
  a <-  as.data.frame(colSums(a[,-which(names(a) %in% c("Characteristic", "Topic"))])) # Remove characteristic column, leaving only vector of values
  colnames(a) <- new.col.name # rename colname to characteristic 
  return(a)
}

census <- read.csv("2016_neighbourhood_profiles.csv") # read census data

# Clean 2016 neighbourhood profile dataset to get variables---- 
# USing the 2016 neighbourhood profiles data to get 2016 data
census <- as.data.frame(census)

# Remove Hood_ID as row, and data source column
census <- census[!(census$Characteristic == "TSNS2020 Designation"), -which(names(census) %in% c("Category", "Data.Source"))]
str(census)

# Create a dataframe of codes for each neighbourhood
# Extract neighbouorhood names and Hood_Ids from census data, excluding necessary columns and rows
neigh.codes <- as.data.frame(cbind(colnames(census[, -which(names(census) %in% c("Topic", "Characteristic", "City.of.Toronto"))]), as.vector(unlist(census[census$Characteristic == "Neighbourhood Number", -which(names(census) %in% c("Topic", "Characteristic", "City.of.Toronto"))]))))
colnames(neigh.codes) <- c("Neighbourhood", "Hood_ID") # Rename columns

# Remove thousands seperator commas from numbers and replace % sign eith e-2, 
# so we can use as.numeric to convert from a character to a number
census[, -which(names(census) %in% c("Topic", "Characteristic"))] <- lapply(census[,-which(names(census) %in% c("Topic", "Characteristic"))], function(x) {gsub(",", "", x)}) #remove commas as thousands seperator
census[, -which(names(census) %in% c("Topic", "Characteristic"))] <- lapply(census[, -which(names(census) %in% c("Topic", "Characteristic"))], function(x) {gsub("%", "e-2", x)}) #turn %sign to e-2

# Turn n/as into NA
census[census == "n/a"] <- NA

# Get number of non-Canadian citizens in each neighborhood
non.citizens <- cbind.data.frame(neigh.codes, getData(census.tmp, "Not Canadian citizens", "non.citizens"))
