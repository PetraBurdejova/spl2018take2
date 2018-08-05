[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Creating the getData Function to Gather Data** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : getData Function


Description: Creating the Function used to gather data from the census dataset

Keywords: getData, function, data frame

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```



### R Code
```r

###Create function to get data from main dataset
getData <- function(x, characteristic, new_col_name = characteristic) {
  a <- subset(x, Characteristic == characteristic) #subset dataframe by characteristic
  a <-  as.data.frame(colSums(a[,-which(names(a) %in% c("Characteristic", "Topic"))])) #Remove characteristic column, leaving only vector of values
  colnames(a) <- new_col_name #rename colname to characteristic 
  return(a)
  }

####Get number of low income people in each neighbourhood
low.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "low.income")) #get no of people classified as low income
agg.2016 <- join(agg.2016, low.income[, -which(names(low.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of low income people to agg.2016

####Get number of middle income people in each neighbourhood
middle.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "middle.income")) #get no of people classified as middle income
agg.2016 <- join(agg.2016, middle.income[, -which(names(middle.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of middle income people to agg.2016

####Get number of high income people in each neighbourhood
high.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "high.income")) #get no of people classified as high income
agg.2016 <- join(agg.2016, high.income[, -which(names(high.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of high income people to agg.2016

