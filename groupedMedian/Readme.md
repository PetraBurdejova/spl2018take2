[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Calculating Median Income from Grouped Income Data** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Median Income


Description: Calculating median income from grouped inocme data

Keywords: median income, function, Toronto

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```



### R Code
```r

###Find Median Income
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Income of individuals in 2015", ] #subset census data by income
census.tmp <- census.tmp[c(35:47),] #need to subset by rows because characteristic is duplicated for other income groups
census.tmp <- census.tmp[!census.tmp$Characteristic == "$100,000 and over",]


###Change Characteristic Vector to specific form
census.tmp$Characteristic <- c("0-9999", "10000-19999", "20000-29999", "30000-39999", "40000-49999", "50000-59999",
                        "60000-69999", "70000-79999", "80000-89999", "90000-99999", "100000-149999", "150000-1000000") #create income intervals increasing by 10000, last interval has max of 1000000 as assumption 

###Create Function to Calculate median income using groups
Grouped_Median <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  Midpoints <- rowMeans(intervals) #midpoint of interval
  cf <- cumsum(frequencies) 
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of median class
  h <- diff(intervals[, Midrow]) # size of median class
  f <- frequencies[Midrow]       # frequency of median class
  cf2 <- cf[Midrow - 1]          # cumulative frequency class before median class
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - cf2)/f * h)
}

median.income <- cbind.data.frame(neigh.codes,  #apply groued median function to income intervals from census data
                                  as.data.frame(sapply(census.tmp[, -which(names(census.tmp) %in% c("Topic", "Characteristic"))], function(x) {Grouped_Median(x,intervals = census.tmp$Characteristic, sep = "-")})))
                                  
colnames(median.income) <- c(colnames(neigh.codes), "median.income") #rename columns

agg.2016 <- join(agg.2016, median.income[, -which(names(median.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join median income to agg.2016

