
a <- read.csv("MCI_2014_to_2017.csv")
b <- read.csv("2016_neighbourhood_profiles.csv")
drugs <- read.csv("toronto_drug_arrests.csv")
wbt <- read.csv("wellbeing_toronto.csv")

#Create aggregate data frame using data table
a.dt <- as.data.frame(a)

#Remove duplicated event IDS
a.dt <- subset(a.dt, !duplicated(a.dt$event_unique_id))

#Filter out occurrence dates before so we only look at 2016
a.dt[,c("occurrencedate", "reporteddate")] <- lapply(a.dt[,c("occurrencedate", "reporteddate")], as.Date)
a.dt <- a.dt %>%
           filter(occurrencedate >= as.Date("2016-01-01") & occurrencedate < as.Date("2017-01-01"))
a.dt <- a.dt[complete.cases(a.dt), ]

####Aggregate crimes by type
a.dt <- as.data.table(a.dt)
setkey(a.dt, "MCI", "Hood_ID")
agg <- a.dt[, .(count = .N), by = c("MCI", "Hood_ID")]
agg <- dcast(agg, Hood_ID ~ MCI)
agg$Hood_ID <- as.factor(agg$Hood_ID)

#Turn Nas to 0 for crimes that didn't occur in that neighbourhood
agg[is.na(agg)] <- 0


###Add Drug Arrests to aggregate dataset
drugs$Neighbourhood.Id <- as.factor(drugs$Neighbourhood.Id)
agg <- merge(agg, drugs[,c("Neighbourhood.Id", "Drug.Arrests")], by.x = "Hood_ID", by.y = "Neighbourhood.Id" )

###GEt crime totals for each neighbourhood
agg$Total.crime <- rowSums(agg[,-1])

##Merge agg with wbt to get crime and neighbourhood profiles in 1 data frame
wbt$Neighbourhood.Id <- as.factor(wbt$Neighbourhood.Id)
agg <- merge(agg, wbt, by.x = "Hood_ID", by.y = "Neighbourhood.Id")
agg[,c("Neighbourhood", "Combined.Indicators")] <- NULL


###USing the 2016 neighbourhood profiles data to get 2016 data
df1 <- as.data.frame(b)
df1 <- b[-c(1,2),-c(1,3)]
str(df1)

###Create a dataframe of codes for each neighbourhood
neigh.codes <- as.data.frame(cbind(colnames(df1[,-c(1,2)]), as.vector(unlist(b[1,-c(1:4)]))))
neigh.codes <- neigh.codes[-1,] 
colnames(neigh.codes) <- c("Neighborhood", "Hood_ID")

#####Remove thousands seperator commas from numbers and replace % sign eith e-2, 
####so we can use as.numeric to convert from a character to a number
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], function(x) {gsub(",", "", x)})
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], function(x) {gsub("%", "e-2", x)})

###Turn n/as into NA
df1[df1 == "n/a"] <- NA

####Turn columns into numeric
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], as.numeric)
df1[,c("Topic", "City.of.Toronto")] <- NULL
df2 <- as.data.frame(df1)

####Get number of males from 15 - 35 in 2016
df2$Characteristic <- fct_collapse(df2$Characteristic,
                         male.15.to.35 = c("Male: 15 to 19 years", "Male: 20 to 24 years", "Male: 25 to 29 years", "Male: 30 to 34 years")
                    )

male.15.to.30 <- subset(df2, Characteristic == "male.15.to.35")
male.15.to.30 <- as.data.frame(colSums(male.15.to.30[,-1]))
colnames(male.15.to.30) <- c("males.15.to.30")

####Get number of males in 2016
df2 <- as.data.frame(df1)

df2$Characteristic <- fct_collapse(df2$Characteristic,
                                   male.above.15 = c("Male: 15 to 19 years", "Male: 20 to 24 years", "Male: 25 to 29 years", 
                                                     "Male: 30 to 34 years", "Male: 35 to 39 years", "Male: 40 to 44 years", 
                                                     "Male: 45 to 49 years", "Male: 50 to 54 years", "Male: 55 to 59 years",
                                                     "Male: 60 to 64 years", "Male: 65 to 69 years", "Male: 70 to 74 years",
                                                     "Male: 75 to 79 years", "Male: 80 to 84 years", "Male: 85 to 89 years",
                                                     "Male: 90 to 94 years", "Male: 95 to 99 years", "Male: 100 years and over"),
                                   
                                   female.above.15 = c("Female: 15 to 19 years", "Female: 20 to 24 years", "Female: 25 to 29 years", 
                                                       "Female: 30 to 34 years", "Female: 35 to 39 years", "Female: 40 to 44 years", 
                                                       "Female: 45 to 49 years", "Female: 50 to 54 years", "Female: 55 to 59 years",
                                                       "Female: 60 to 64 years", "Female: 65 to 69 years", "Female: 70 to 74 years",
                                                       "Female: 75 to 79 years", "Female: 80 to 84 years", "Female: 85 to 89 years",
                                                       "Female: 90 to 94 years", "Female: 95 to 99 years", "Female: 100 years and over"))
#SUbset males above 15 years old
male.above.15 <- subset(df2, Characteristic == "male.above.15")
male.above.15 <- as.data.frame(colSums(male.above.15[,-1]))
colnames(male.above.15) <- c("male.above.15")

#SUbset females above 15 years old
female.above.15 <- subset(df2, Characteristic == "female.above.15")
female.above.15 <- as.data.frame(colSums(female.above.15[,-1]))
colnames(female.above.15) <- c("female.above.15")

#Get population for 2016
df2 <- as.data.frame(df1)

df2$Characteristic <- fct_collapse(df2$Characteristic,
                                   population.2016 = c("Population, 2016"))

population.2016 <- subset(df2, Characteristic == "population.2016")
population.2016 <- as.data.frame(colSums(population.2016[,-1]))
colnames(population.2016) <- c("population.2016")

#####Get income groups
df2 <- as.data.frame(df1)

df2 <- df2[c(968:980),]
df2 <- df2[!df2$Characteristic == "$100,000 and over",]
df2$Characteristic <- fct_collapse(df2$Characteristic,
                                   low.income = c("Under $10,000 (including loss)", "$10,000 to $19,999",
                                                  "$20,000 to $29,999", "$30,000 to $39,999"),
                                   middle.income = c("$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999",
                                                     "$70,000 to $79,999", "$80,000 to $89,999"),
                                   high.income = c("$90,000 to $99,999", "$100,000 to $149,999", "$150,000 and over"))

####GEt number of low income people in each neighbourhood
low.income <- subset(df2, Characteristic == "low.income")
low.income <- as.data.frame(colSums(low.income[,-1]))
colnames(low.income) <- c("low.income")

####GEt number of middle income people in each neighbourhood
middle.income <- subset(df2, Characteristic == "middle.income")
middle.income <- as.data.frame(colSums(middle.income[,-1]))
colnames(middle.income) <- c("middle.income")

####GEt number of high income people in each neighbourhood
high.income <- subset(df2, Characteristic == "high.income")
high.income <- as.data.frame(colSums(high.income[,-1]))
colnames(high.income) <- c("high.income")

####Calculate average total income per person for 2016
df2 <- as.data.frame(df1)
df2 <- df2[c(968:980),]
df2 <- df2[!df2$Characteristic == "$100,000 and over",]

###Create vector of incomes that are in the midpoint of the income groups
middle.groups <- c(seq(5000, 95000, by = 10000), 125000, 575000)
total.with.income <- colSums(df2[,-1])

df2[,-1] <- lapply(df2[-1], function(x) {x * middle.groups})
avg.income <- as.data.frame(colSums(df2[,-1]) / total.with.income)
colnames(avg.income) <- c("avg.income")

###Find Median Income
df2 <- as.data.frame(df1)
df2 <- df2[c(968:980),]
df2 <- df2[!df2$Characteristic == "$100,000 and over",]

###Change Characteristic Vector to specific form
df2$Characteristic <- c("0-9999", "10000-19999", "20000-29999", "30000-39999", "40000-49999", "50000-59999",
                        "60000-69999", "70000-79999", "80000-89999", "90000-99999", "100000-149999", "150000-1000000")

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
  
  Midpoints <- rowMeans(intervals)
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of median class
  h <- diff(intervals[, Midrow]) # size of median class
  f <- frequencies[Midrow]       # frequency of median class
  cf2 <- cf[Midrow - 1]          # cumulative frequency class before median class
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - cf2)/f * h)
}

median.income <- as.data.frame(sapply(df2[,-1], function(x) {Grouped_Median(x,intervals = df2$Characteristic, sep = "-")}))
colnames(median.income) <- "median.income"

#####Calculate number of Households in bottom 20% of Income distribution
df2 <- as.data.frame(df1)
df2 <- df2[c(1106:1116),]
df2 <- df2[!df2$Characteristic == "In the top half of the distribution",]

##Calculate sum of households in bottom 20% of income distribution
no.hholds.bottom.20per <- as.data.frame(colSums(df2[df2$Characteristic == "In the bottom decile" | df2$Characteristic == "In the second decile", -1]))
colnames(no.hholds.bottom.20per) <- "no.hholds.bottom.20per" 

##Calculate percentage of households in bottom 20% of income distribution
per.hholds.bottom.20per <- round(no.hholds.bottom.20per / colSums(df2[,-1]), 2)


###TESTING

agg <- read.csv("aggregated.csv")
get.data <- function(x, z, y) {
  m <- as.data.frame(cbind(unlist(x[x$Characteristic == y, -c(1:2)]), neigh.codes$Hood_ID))
  colnames(m) <- c(y, "Hood_ID")
  p <- merge(m,z, by.x = "Hood_ID", by.y = "Hood_ID")
  return(p)
}
y <- c("Population, 2016", "Population, 2011", "Male: 0 to 04 years", "Male: 05 to 09 years", "Total private dwellings")
for(i in y){
  agg <- get.data(df1, agg, i)
}

###end test#### -> works perfectly 

c <- read_csv("wellbeing_toronto.csv")
d <- read_csv("toronto_drug_arrests.csv")

df <- c %>% select(- c(1:3)) %>% bind_cols(agg, .)
df <- d %>% select(- c(1:3)) %>% bind_cols(df, .)


reg1 <- lm(Assault~ Unemployed, data = df)
res1 <- residuals(reg1)
plot(res1)

####calculate percentage rates cp. to pop.size####
agg <- df

pcts = lapply(agg[,-c(1,5,24,22)], function(x) {
  x / agg$`Population, 2016`
})
pcts <- as.data.frame(pcts)

pcts$Hood_id <- agg$Hood_ID
pcts$HFI <- agg$`Healthy Food Index`
pcts$EDI <- agg$`Early Development Instrument (EDI)`

df1 <- pcts

scatter.smooth(df1$Unemployed, df1$Assault)

reg2 <- lm(Assault~Unemployed, data = df1)
summary(reg2)
res2 <- residuals(reg2)
plot(res2)


reg3 <- lm(Assault ~ Youth.15.24 + Visible.Minority.Category + In.Labour.Force + With.Bachelor.Degree.or.Higher ,data = df1)
summary(reg3)
plot(reg3)



