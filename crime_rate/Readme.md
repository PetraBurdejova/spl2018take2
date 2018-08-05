[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Calculating Crime Rates per 10 000** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Crime Rates per 10 000


Description: Calculating crime rates per 10 000

Keywords: crime rates, function, Toronto

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```



### R Code
```r

#create a function to turn crime variable into crime per hundred thousand rate
crime.per.tenthsnd <- function (x) {
  x / agg.2016[, "population.2016"] * 10000
}

#define crime variables
crime.vars <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")

#use for loop to join crime per ten thousand rates to agg.2016
for (i in crime.vars) {
  agg.2016[, paste(i, ".per.tenthsnd", sep = "", collapse = NULL)] <- crime.per.tenthsnd(agg.crime[, i, with = FALSE])
}
