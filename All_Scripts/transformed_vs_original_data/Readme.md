[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Original and transformed dependent variables** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Histograms - Transformed vs Original Data


Description: Create average shifted histograms for original and transformed dependent variables

Keywords: transformation, normal distribution

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi


```
![Picture1](transformed_vs_orig_data_1.pdf)
![Picture2](transformed_vs_orig_data_2.pdf)



### R Code
```r
source("../spl2018take2/All_scripts/Merging/Merging.R")

r <- as.data.frame(agg.2016)
r$obsnumber <- r$Hood_ID # set Hood_ID as observation number


# create the FindBestExponent-function
swD <- function (p, x) {
  # finds the best exponent for a basic power transformation towards normality
  # arguments:
  # p: the exponent used for the basic power transformation
  # x: the data we want to transform
  # returns:
  # the p-value of the Shapiro-Wilk test applied on the transformed data
  y <- bcPower(x, p)
  shapiro.test(y)$statistic
}

par(mfrow = c(3,3))


for (i in crime.vars) {
  r$tmp <- r[, i] 
  
  outliers <- r[r$tmp>mean(r$tmp)+2.5*IQR(r$tmp), ]$obsnumber # find obervation numbers greater than 2.5 * interquartile range
  
  outliers
  
  rtmp <- r[!r$obsnumber %in% outliers, ] #remove outliers
  
  rtmp[rtmp$tmp == 0, ] <- 1 #turn zero values to 1
  
  exponent <- optimize(swD, c(-3,3), x = rtmp$tmp)$objective
  
  shapiro.test(bcPower(rtmp$tmp,exponent))
  
  rtmp$tmp.bp <- bcPower(rtmp$tmp, exponent)
  
  # plot original data
  plot(ash1(bin1(r$tmp)), main = paste(i,"(original)"), xlab = paste("Number of", i),
       ylab = "Frequency", type = "s", mgp = c(2.5,1,0), lwd = 2)
  
  x <- seq(min(r$tmp), max(r$tmp), length.out = 100)
  
  lines(x, dnorm(x, mean = mean(r$tmp), sd = sd(r$tmp)), col = "red", lwd = 2)
  
  shapiro.test(r$tmp)$p.value
  
  mtext(paste("p-value =", round(shapiro.test(r$tmp)$p.value, 2)), line = 0.4, cex = 0.9, col = "red", font = 2)
  
  # transformed
  plot(ash1(bin1(rtmp$tmp.bp)), main = paste(i, "(transformed)"),
       xlab = paste("Number of", i, "(transformed)"), ylab = "Frequency", type = "s",
       mgp = c(2.5,1,0), lwd = 2)
  x <- seq(min(rtmp$tmp.bp), max(rtmp$tmp.bp), length.out = 100)
  lines(x, dnorm(x, mean = mean(rtmp$tmp.bp), sd = sd(rtmp$tmp.bp)), col = "red", 
        lwd = 2)
  shapiro.test(rtmp$tmp.bp)$p.value
  mtext(paste("p-value =", round(shapiro.test(r$tmp)$p.value, 2)), line = 0.4, cex = 0.9, col = "red", font = 2)
}

par(mfrow = c(1,1))
