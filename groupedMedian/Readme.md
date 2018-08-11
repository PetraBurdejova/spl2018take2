[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Calculating Median Income from Grouped Income Data** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Median Income


Description: Calculating median income from grouped income data

Keywords: median income, function, Toronto

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```



### R Code
```r

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

# Find Median Income
census.tmp <- as.data.frame(census) # Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Income of individuals in 2015", ] # subset census data by income
census.tmp <- census.tmp[c(35:47),] # need to subset by rows because characteristic is duplicated for other income groups
census.tmp <- census.tmp[!census.tmp$Characteristic == "$100,000 and over",]

# Change Characteristic Vector to specific form
census.tmp$Characteristic <- c("0-9999", "10000-19999", "20000-29999", "30000-39999", 
                               "40000-49999", "50000-59999", "60000-69999", "70000-79999", 
                               "80000-89999", "90000-99999", "100000-149999", "150000-1000000") #create income intervals increasing by 10000, last interval has max of 1000000 as assumption 


GroupedMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # Calculate median income using income groups
  #
  # Args: 
  #   frequencies: vector with number of observations in each interval
  #   intervals: intervals used in the distribution
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  #
  # Returns: a vector with median incomes for each neighbourhood
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  Midpoints <- rowMeans(intervals) # midpoint of interval
  cf <- cumsum(frequencies) # cumulative sums of frequencies
  Midrow <- findInterval(max(cf)/2, cf) + 1 # find middle point
  L <- intervals[1, Midrow]      # lower class boundary of median class
  h <- diff(intervals[, Midrow]) # size of median class
  f <- frequencies[Midrow]       # frequency of median class
  cf2 <- cf[Midrow - 1]          # cumulative frequency class before median class
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - cf2)/f * h) # formula to calculate median 
}

median.income <- cbind.data.frame(neigh.codes,  # apply grouped median function to income intervals from census data
                                  as.data.frame(sapply(census.tmp[, -which(names(census.tmp) %in% c("Topic", "Characteristic"))], function(x) {GroupedMedian(x,intervals = census.tmp$Characteristic, sep = "-")})))
