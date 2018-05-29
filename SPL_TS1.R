if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE); library("tidyverse")
if(!require("xts")) install.packages("xts", dependencies = TRUE); library("xts")
if(!require("zoo")) install.packages("zoo", dependencies = TRUE); library("zoo")
if(!require("chron")) install.packages("chron", dependencies = TRUE); library("chron")
if(!require("forecast")) install.packages("forecast", dependencies = TRUE); library("forecast")


setwd("C:/Users/Felix/Desktop/Uni/Berlin/2. Semester/spl2018")

a <- read_csv("MCI_2014_to_2017.csv")
b <- read_csv("2016_neighbourhood_profiles.csv")


#few values from 2004 and so on..., ts actually starts in 2014 
a_orig <- a
a <- a[!a$occurrencedate <= "2014-01-01",]


MyDatesTable <- table(cut.POSIXt(a$occurrencedate, breaks="day"))
tail(MyDatesTable, 500)


c <- c()
c$occurrencedate <- a$occurrencedate
c$freq <- MyDatesTable

plot(c$freq, type = "l", xlab = "Time", ylab = "Freq")

##start investigation

acf_a <- Acf(c$freq)
pacf_a <- Pacf(c$freq)


