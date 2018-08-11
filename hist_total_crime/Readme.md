[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Histogram of Total Crime** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Histogram of Total Crime


Description: Plots a histogram of the total crime in Toronto 2016.

Keywords: plot, vizualization

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```

![Picture1](hist_total_crime.pdf)


### R Code
```r
source("Merging.R")

# histogram function
HistFunc <- function(x, y, bin.width) {
  # function to quickly print ggplot histograms and specify bin widths
  #
  # Args:
  #
  #   x: data frame or table to be used
  #   y: column with data to be plotted as histogram
  #   bin.width: bin width to split continuous variable into intervals
  #
  #   Returns: a histogram of the specified variable
  Max <- max(x[[y]]) # max value of variable
  print(ggplot(data = x, aes_string(y)) + # select data frameto be used and aesthetic variable is variable to be plotted
        geom_histogram(breaks = seq(0, Max, by = bin.width), col="black", fill ="blue", alpha = .5) + # Set max value onn scale to be max value of variable and bin width 
        labs(title = paste("Histogram of", y, sep = " ", collapse = NULL)) + # Add title
        labs(x = y, y = "Count") + #Add x and y labels
        xlim(c(0, Max)))  #Set min and max values on x label
  
}

# Histogram of total crime commited
HistFunc(agg.crime, "total.crime", 50) # create histogram of total crime, bin width of 50