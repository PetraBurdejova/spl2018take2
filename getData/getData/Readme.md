[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Creating the getData function for merging data** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Get Data Function


Description: Creating the function to get data from census dataset.

Keywords: function, get data, data frame

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

####GEt number of low income people in each neighbourhood
low.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "low.income")) #get no of people classified as low income

agg.2016 <- join(agg.2016, low.income[, -which(names(low.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of low income people to agg.2016
