###load and filter datasets----
crime <- read.csv("MCI_2014_to_2017.csv")
census <- read.csv("2016_neighbourhood_profiles.csv")
drugs <- read.csv("toronto_drug_arrests.csv")
wbt <- read.csv("wellbeing_toronto.csv")
area <- readr::read_csv("toronto_area.csv")

###Change name of X coordinate column to X
colnames(crime)[which(names(crime) == "ï..X")] <- "X"

#Create aggregate data frame 
crime.dt <- as.data.frame(crime)

library(dplyr)
#Remove duplicated event IDS
crime.dt <- subset(crime.dt, !duplicated(crime.dt$event_unique_id))

#Filter out occurrence dates before so we only look at 2016
crime.dt[,c("occurrencedate", "reporteddate")] <- lapply(crime.dt[,c("occurrencedate", "reporteddate")], as.Date)
crime.dt <- crime.dt %>%  filter(occurrencedate >= as.Date("2016-01-01") & occurrencedate < as.Date("2017-01-01"))
crime.dt <- crime.dt[complete.cases(crime.dt), ] #use only complete cases

####Aggregate crimes by type----
crime.dt <- as.data.table(crime.dt) #Use data table
setkey(crime.dt, "MCI", "Hood_ID") #set crime indicators and Hood_ID as keys
agg <- crime.dt[, .(count = .N), by = c("MCI", "Hood_ID")] #aggregate crime by MCI count and Hood_ID
agg <- dcast(agg, Hood_ID ~ MCI) #turn data from long format to wide
agg$Hood_ID <- as.factor(agg$Hood_ID) #set Hood_ID as factor

#Turn Nas to 0 for crimes that didn't occur in that neighbourhood
agg[is.na(agg)] <- 0


###Add Drug Arrests to aggregate dataset----
drugs$Neighbourhood.Id <- as.factor(drugs$Neighbourhood.Id) #turn Neighbourhood.Id to factor
agg <- merge(agg, drugs[,c("Neighbourhood.Id", "Drug.Arrests")], by.x = "Hood_ID", by.y = "Neighbourhood.Id" ) #merge agg and drugs dataset

###Get crime totals for each neighbourhood and rename columns
agg$Total.crime <- rowSums(agg[,!c("Hood_ID")])
colnames(agg) <- c("Hood_ID", "assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")

##Merge agg with wbt to get crime and neighbourhood profiles in 1 data frame
wbt$Neighbourhood.Id <- as.factor(wbt$Neighbourhood.Id)
agg.2014 <- merge(agg, wbt, by.x = "Hood_ID", by.y = "Neighbourhood.Id")
agg.2014[,c("Neighbourhood", "Combined.Indicators")] <- NULL

###Clean 2016 neighbourhood profile dataset to get variables---- 
###USing the 2016 neighbourhood profiles data to get 2016 data
census <- as.data.frame(census)

#Remove Hood_ID as row, and data source column
census <- census[!(census$Characteristic == "TSNS2020 Designation"), -which(names(census) %in% c("Category", "Data.Source"))]
str(census)

###Create a dataframe of codes for each neighbourhood
#Extract neighbouorhood names and Hood_Ids from census data, excluding necessary columns and rows
neigh.codes <- as.data.frame(cbind(colnames(census[, -which(names(census) %in% c("Topic", "Characteristic", "City.of.Toronto"))]), as.vector(unlist(census[census$Characteristic == "Neighbourhood Number", -which(names(census) %in% c("Topic", "Characteristic", "City.of.Toronto"))]))))
colnames(neigh.codes) <- c("Neighbourhood", "Hood_ID") #Rename columns

#####Remove thousands seperator commas from numbers and replace % sign eith e-2, 
####so we can use as.numeric to convert from a character to a number
census[, -which(names(census) %in% c("Topic", "Characteristic"))] <- lapply(census[,-which(names(census) %in% c("Topic", "Characteristic"))], function(x) {gsub(",", "", x)}) #remove commas as thousands seperator
census[, -which(names(census) %in% c("Topic", "Characteristic"))] <- lapply(census[, -which(names(census) %in% c("Topic", "Characteristic"))], function(x) {gsub("%", "e-2", x)}) #turn %sign to e-2

###Turn n/as into NA
census[census == "n/a"] <- NA

###Create function to get data from main dataset
getData <- function(x, characteristic, new_col_name = characteristic) {
  a <- subset(x, Characteristic == characteristic) #subset dataframe by characteristic
  a <-  as.data.frame(colSums(a[,-which(names(a) %in% c("Characteristic", "Topic"))])) #Remove characteristic column, leaving only vector of values
  colnames(a) <- new_col_name #rename colname to characteristic 
  return(a)
  }


####Turn columns into numeric
census[, -which(names(census) %in% c("Topic", "Characteristic"))] <- lapply(census[, -which(names(census) %in% c("Topic", "Characteristic"))], as.numeric) #turn numeric columns into numeric class
census <- census[!census$Characteristic == "Neighbourhood Number", ] #Remove neighbourhood ID from census data
census[, c("City.of.Toronto")] <- NULL #remove topic and city-wide columns

###Create dataframe to aggregate all variables from 2016 neighbourhod profiles dataset
agg.2016 <- cbind.data.frame(Hood_ID = neigh.codes$Hood_ID)

###Age and Gender variables----
library(forcats)
###Get number of males from 15 - 24 in 2016
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Age characteristics", ] #subset census data by age characteristics
census.tmp$Characteristic <- fct_collapse(census.tmp$Characteristic,
                         male.youth = c("Male: 15 to 19 years", "Male: 20 to 24 years") #Collapse males 15-24 into 1 category
                    )

male.youth <- cbind.data.frame(neigh.codes, getData(census.tmp, "male.youth"))
agg.2016 <- join(agg.2016, male.youth[, -which(names(male.youth) %in% c("Neighbourhood"))], by = "Hood_ID") #join to agg.2016 dataframe

###Get number of youth (15 - 24) in 2016

census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Age characteristics", ] #subset census data by age characteristics
census.tmp$Characteristic <- fct_collapse(census.tmp$Characteristic,
                                   youth = c("Male: 15 to 19 years", "Male: 20 to 24 years", "Female: 15 to 19 years", "Female: 20 to 24 years")
)

youth <- cbind.data.frame(neigh.codes, getData(census.tmp, "youth")) #get data for number of youth in each neighbourhood
agg.2016 <- join(agg.2016, youth[, -which(names(male.youth) %in% c("Neighbourhood"))], by = "Hood_ID") #join to agg.2016 dataframe


####Get number of males in 2016
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Age characteristics", ] #subset census data by age characteristics
census.tmp$Characteristic <- fct_collapse(census.tmp$Characteristic,
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
male.above.15 <- cbind.data.frame(neigh.codes, getData(census.tmp, "male.above.15")) #get no. of males for each neighbourhood
agg.2016 <- join(agg.2016, male.above.15[, -which(names(male.above.15) %in% c("Neighbourhood"))], by = "Hood_ID") #join male.above.15 to agg.2016

#SUbset females above 15 years old
female.above.15 <- cbind.data.frame(neigh.codes, getData(census.tmp, "female.above.15")) #get no. of females for each neighbourhood
agg.2016 <- join(agg.2016, female.above.15[, -which(names(female.above.15) %in% c("Neighbourhood"))], by = "Hood_ID") #join female.above.15 to agg.2016

#Get population for 2016
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Population and dwellings", ] #subset census data by population and dwellings
census.tmp$Characteristic <- fct_collapse(census.tmp$Characteristic,
                                   population.2016 = c("Population, 2016")) #rename Population, 2016

population.2016 <- cbind.data.frame(neigh.codes, getData(census.tmp, "population.2016")) #get population 2016 data for each neighbourhood
agg.2016 <- join(agg.2016, population.2016[, -which(names(population.2016) %in% c("Neighbourhood"))], by = "Hood_ID") #join data population.2016 to agg.2016

#create a function to turn crime variable into crime per hundred thousand rate
crime.per.tenthsnd <- function (x) {
  x / agg.2016[, "population.2016"] * 10000
}

#define crime variables
crime.vars <- c("assault", "auto.theft", "break.and.enter", "robbery", "theft.over", "drug.arrests", "total.crime")

#use for loop to join crime per ten thousand rates to agg.2016
for (i in crime.vars) {
  agg.2016[, paste(i, ".per.tenthsnd", sep = "", collapse = NULL)] <- crime.per.tenthsnd(agg.2016[, i])
}

###Get area and density of each neighbourhood
colnames(area) <- c("Neighbourhood", "Hood_ID", "total.area")
agg.2016 <- join(agg.2016, area[, -which(names(area) %in% c("Neighbourhood"))], by = "Hood_ID") #join area to agg.2016
agg.2016$density <- agg.2016$population.2016 / agg.2016$total.area #calculate density = pop/area

###Get Lone Parent Families by sex of parent-----
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Family characteristics", ] #subset census data by family characteristics

lone.parent.families <- cbind.data.frame(neigh.codes, getData(census.tmp, "Total lone-parent families by sex of parent", "lone.parent.families")) #get no. of loneparent families
agg.2016 <- join(agg.2016, lone.parent.families[, -which(names(lone.parent.families) %in% c("Neighbourhood"))], by = "Hood_ID") #join to agg.2016

###percent of lone parent families
lone.parent.families.per <- cbind.data.frame(neigh.codes, (getData(census.tmp, "Total lone-parent families by sex of parent", "lone.parent.families.per") /  #divide no. of lone parent families by total no. of census families
                                getData(census.tmp, "Total number of census families in private households") * 100))

agg.2016 <- join(agg.2016, lone.parent.families.per[, -which(names(lone.parent.families.per) %in% c("Neighbourhood"))], by = "Hood_ID") #join to agg.2016

###Get Income characteristices of each neighbourhood----
#####Get income groups
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Income of individuals in 2015", ] #subset census data by income
census.tmp <- census.tmp[c(31:47),] #need to subset by rows because characteristic is duplicated for other income groups
census.tmp <- census.tmp[!census.tmp$Characteristic == "$100,000 and over",]

census.tmp$Characteristic <- fct_collapse(census.tmp$Characteristic,     #reclassify income groups as high, middle or low
                                   low.income = c("Under $10,000 (including loss)", "$10,000 to $19,999",
                                                  "$20,000 to $29,999", "$30,000 to $39,999"),
                                   middle.income = c("$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999",
                                                     "$70,000 to $79,999", "$80,000 to $89,999"),
                                   high.income = c("$90,000 to $99,999", "$100,000 to $149,999", "$150,000 and over"))

####GEt number of low income people in each neighbourhood
low.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "low.income")) #get no of people classified as low income
agg.2016 <- join(agg.2016, low.income[, -which(names(low.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of low income people to agg.2016

####GEt number of middle income people in each neighbourhood
middle.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "middle.income")) #get no of people classified as middle income
agg.2016 <- join(agg.2016, middle.income[, -which(names(middle.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of middle income people to agg.2016

####GEt number of high income people in each neighbourhood
high.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "high.income")) #get no of people classified as high income
agg.2016 <- join(agg.2016, high.income[, -which(names(high.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of high income people to agg.2016

###Get average income
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Income sources", ] #subset census data by income sources

avg.income <- cbind.data.frame(neigh.codes, getData(census.tmp, "Total income: Average amount ($)", "avg.income")) #get avg income for each neighbourhood
agg.2016 <- join(agg.2016, avg.income[, -which(names(avg.income) %in% c("Neighbourhood"))], by = "Hood_ID") #join avg income to agg.2016

###number of people taking unemployment benefits (EI)
people.ei <- cbind.data.frame(neigh.codes, getData(census.tmp, "Employment Insurance (EI) benefits: Population with an amount", "people.ei")) #get no of people who recieve employment insurance
agg.2016 <- join(agg.2016, people.ei[, -which(names(people.ei) %in% c("Neighbourhood"))], by = "Hood_ID") #join no of people taking EI to agg.2016

###percent of people taking unemployment benefits (EI)
people.ei.per <- cbind.data.frame(neigh.codes, (getData(census.tmp, "Employment Insurance (EI) benefits: Population with an amount", "people.ei.per") /
                    getData(census.tmp, "Total income: Population with an amount") * 100)) #no of prople receiving EI/no of people with an income
agg.2016 <- join(agg.2016, people.ei.per[, -which(names(people.ei.per) %in% c("Neighbourhood"))], by = "Hood_ID") #join people.ei.per to agg.2016

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

#####Calculate number of Households in bottom 20% of Income distribution
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Income of economic families in 2015", ] #subset census data by income of economic families
census.tmp <- census.tmp[!census.tmp$Characteristic == "In the top half of the distribution",]

##Calculate sum of households in bottom 20% of income distribution
no.hholds.bottom.20per <- cbind.data.frame(neigh.codes, 
                                           as.data.frame(colSums(census.tmp[census.tmp$Characteristic == "In the bottom decile" | census.tmp$Characteristic == "In the second decile", -which(names(census.tmp) %in% c("Topic", "Characteristic"))])))
colnames(no.hholds.bottom.20per) <- c(colnames(neigh.codes), "no.hholds.bottom.20per") 
agg.2016 <- join(agg.2016, no.hholds.bottom.20per[, -which(names(no.hholds.bottom.20per) %in% c("Neighbourhood"))], by = "Hood_ID")

##Calculate percentage of households in bottom 20% of income distribution
hholds.bottom.20per.per <- cbind.data.frame(neigh.codes,  #no of hholds in bottom 20 per / total no of households
                                            round(no.hholds.bottom.20per[, "no.hholds.bottom.20per"] / getData(census.tmp, "Total - Economic family income decile group for the population in private households - 100% data", "hholds.bottom.20per.per"), 2) * 100)
agg.2016 <- join(agg.2016, hholds.bottom.20per.per[, -which(names(hholds.bottom.20per.per) %in% c("Neighbourhood"))], by = "Hood_ID") #join to agg.2016

###Get number of low income individuals
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Low income in 2015", ] #subset census data by low income

#Total number of low income individuals according to low income measure
low.income.pop <- cbind.data.frame(neigh.codes, #get data of no of people classified as low income
                                   getData(census.tmp, "In low income based on the Low-income measure, after tax (LIM-AT)", "low.income.pop"))
agg.2016 <- join(agg.2016, low.income.pop[, -which(names(low.income.pop) %in% c("Neighbourhood"))], by = "Hood_ID") #join to agg.2016

###Number of low income individuals according to low income measure 18-64 years
census.tmp <- census.tmp[c(6:15), ] #subset census data because characterisitic shows up more than once
low.income.pop.18.to.64 <- cbind.data.frame(neigh.codes, getData(census.tmp, "18 to 64 years", "low.income.pop.18.to.64")) #get number of people aged 18-64 in low income
agg.2016 <- join(agg.2016, low.income.pop.18.to.64[, -which(names(low.income.pop.18.to.64) %in% c("Neighbourhood"))], by = "Hood_ID")

###Percentage of low income individuals according to low income measure
low.income.pop.per <- cbind.data.frame(neigh.codes, 
                                       getData(census.tmp, "Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)", "low.income.pop.per"))
agg.2016 <- join(agg.2016, low.income.pop.per[, -which(names(low.income.pop.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Percenatage of low income individuals according to low income measure 18-64 years
low.income.pop.18.to.64.per <- cbind.data.frame(neigh.codes, getData(census.tmp, "18 to 64 years (%)", "low.income.pop.18.to.64.per"))
agg.2016 <- join(agg.2016, low.income.pop.18.to.64.per[, -which(names(low.income.pop.18.to.64.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Citizenship and Immigration stats of residents----
###GEt number of non-Canadian citizens in each neighbourhood
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Citizenship", ] #subset census data by citizenship

####Get number of non-Canadian citizens in each neighborhood
non.citizens <- cbind.data.frame(neigh.codes, getData(census.tmp, "Not Canadian citizens", "non.citizens"))
agg.2016 <- join(agg.2016, non.citizens[, -which(names(non.citizens) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get percentage of non-citizens in neighbourhood
non.citizens.per <- cbind.data.frame(neigh.codes, 
                                     getData(census.tmp, "Not Canadian citizens", "non.citizens.per") / getData(census.tmp, "Total - Citizenship for the population in private households - 25% sample data") * 100)
colnames(non.citizens.per) <- c(colnames(neigh.codes), "non.citizens.per")
agg.2016 <- join(agg.2016, non.citizens.per[, -which(names(non.citizens.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get number of immigrants in each neighbourhood
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Immigrant status and period of immigration", ] #subset census data by immigrant status


###Get number of immigrants in each neighbourhood
immigrants <- cbind.data.frame(neigh.codes, getData(census.tmp, "Immigrants", "immigrants"))
agg.2016 <- join(agg.2016, immigrants[, -which(names(immigrants) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get percentage of immigrants in each neighbourhood
immigrants.per <- cbind.data.frame(neigh.codes,
                                   getData(census.tmp, "Immigrants", "immigrants.per") / getData(census.tmp, "Total - Immigrant status and period of immigration for the population in private households - 25% sample data") * 100)
agg.2016 <- join(agg.2016, immigrants.per[, -which(names(immigrants.per) %in% c("Neighbourhood"))], by = "Hood_ID")

####Number of immigrants in last 5 years as recent immigrants
immigrants.recent <- cbind.data.frame(neigh.codes, getData(census.tmp, "2011 to 2016", "immigrants.recent"))
agg.2016 <- join(agg.2016, immigrants.recent[, -which(names(immigrants.recent) %in% c("Neighbourhood"))], by = "Hood_ID")

###percentage of population that are recent immigrants
immigrants.recent.per <- cbind.data.frame(neigh.codes, 
                                          getData(census.tmp, "2011 to 2016", "immigrants.recent.per") / getData(census.tmp, "Total - Immigrant status and period of immigration for the population in private households - 25% sample data") * 100)
agg.2016 <- join(agg.2016, immigrants.recent.per[, -which(names(immigrants.recent.per) %in% c("Neighbourhood"))], by = "Hood_ID")

####Get number of refugees who landed from 1986-2016 in each neighbourhood
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Admission category and applicant type", ] #subset census data by admission type


####Get number of refugees who landed from 1986-2016 in each neighbourhood
refugees <- cbind.data.frame(neigh.codes, getData(census.tmp, "Refugees", "refugees"))
agg.2016 <- join(agg.2016, refugees[, -which(names(refugees) %in% c("Neighbourhood"))], by = "Hood_ID")

####Get percent of refugees who landed from 1986-2016 in each neighbourhood
refugees.per <- cbind.data.frame(neigh.codes,
                                 getData(census.tmp, "Refugees", "refugees.per") / population.2016[, "population.2016"] * 100) 
agg.2016 <- join(agg.2016, refugees.per[, -which(names(refugees.per) %in% c("Neighbourhood"))], by = "Hood_ID")

####Get number of visible minorities in each neighbourhood
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Visible minority population", ] #subset census data by visible minority status

####Get number of visible minorities in each neighbourhood
vis.minorities <- cbind.data.frame(neigh.codes, getData(census.tmp, "Total visible minority population", "vis.minorities"))
agg.2016 <- join(agg.2016, vis.minorities[ , -which(names(vis.minorities) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get percent of pop that are visible minorities
vis.minorities.per <- cbind.data.frame(neigh.codes, 
                                       getData(census.tmp, "Total visible minority population", "vis.minorities.per") / getData(census.tmp, "Total - Visible minority for the population in private households - 25% sample data") * 100)
agg.2016 <- join(agg.2016, vis.minorities.per[, -which(names(vis.minorities.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Housing characteristics----
###Get number of renters
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Household characteristics", ] #subset census data by household characteristics

#Get no of renters in each neighbourhood
renters <- cbind.data.frame(neigh.codes, getData(census.tmp, "Renter", "renters"))
agg.2016 <- join(agg.2016, renters[, -which(names(renters) %in% c("Neighbourhood"))], by = "Hood_ID")

####Get percent of renters in each neighbourhood
renters.per <- cbind.data.frame(neigh.codes, 
                                (getData(census.tmp, "Renter", "renters.per") / getData(census.tmp, "Total - Private households by tenure - 25% sample data")) * 100)
agg.2016 <- join(agg.2016, renters.per[, -which(names(renters.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get number of dwellings that are not condominiums in each neighbourhood
houses <- cbind.data.frame(neigh.codes, getData(census.tmp, "Not condominium", "houses"))
agg.2016 <- join(agg.2016, houses[, -which(names(houses) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get percent of buildings that are not condominiums in each neighbourhood
houses.per <- cbind.data.frame(neigh.codes, 
                               getData(census.tmp, "Not condominium", "houses.per") / getData(census.tmp, "Total - Occupied private dwellings by condominium status - 25% sample data") * 100) 
agg.2016 <- join(agg.2016, houses.per[, -which(names(houses.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get number of households living in unsuitable housing conditions for size and makeup of family, 
###according to statscanada definition
unsuitable.housing <- cbind.data.frame(neigh.codes, getData(census.tmp, "Not suitable", "unsuitable.housing"))
agg.2016 <- join(agg.2016, unsuitable.housing[, -which(names(unsuitable.housing) %in% c("Neighbourhood"))], by = "Hood_ID")

###percent hholds in unsuitable housing
unsuitable.housing.per <- cbind.data.frame(neigh.codes,
                                           getData(census.tmp, "Not suitable", "unsuitable.housing.per") / getData(census.tmp, "Total - Private households by housing suitability - 25% sample data") * 100) 
agg.2016 <- join(agg.2016, unsuitable.housing.per[, -which(names(unsuitable.housing.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get households that require major repairs
hhlds.mjr.rprs <- cbind.data.frame(neigh.codes, getData(census.tmp, "Major repairs needed", "hhlds.mjr.rprs"))
agg.2016 <- join(agg.2016, hhlds.mjr.rprs[, -which(names(hhlds.mjr.rprs) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get percent or hhlds that need major repairs
hhlds.mjr.rprs.per <- cbind.data.frame(neigh.codes, 
                                       getData(census.tmp, "Major repairs needed", "hhlds.mjr.rprs.per") / getData(census.tmp, "Total - Occupied private dwellings by dwelling condition - 25% sample data") * 100)
agg.2016 <- join(agg.2016, hhlds.mjr.rprs.per[, -which(names(hhlds.mjr.rprs.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get households that spend 30 percent or more of income on shelter costs
unaffordable.housing <- cbind.data.frame(neigh.codes, getData(census.tmp, "Spending 30% or more of income on shelter costs", "unaffordable.housing"))
agg.2016 <- join(agg.2016, unaffordable.housing[, -which(names(unaffordable.housing) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get percent of households that spend more than 30 percent of income on shelter costs
unaffordable.housing.per <- cbind.data.frame(neigh.codes, getData(census.tmp, "Spending 30% or more of income on shelter costs", "unaffordable.housing.per") /
                                    getData(census.tmp, "Total - Owner and tenant households with household total income greater than zero; in non-farm; non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data") * 100)
agg.2016 <- join(agg.2016, unaffordable.housing.per[, -which(names(unaffordable.housing.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Education of residents-----
###Get data on education level of residents of each neighbourhood
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Highest certificate, diploma or degree", ] #subset census data by education
census.tmp <- census.tmp[c(16:30), ] #Need to subset further by rows because of duplication of characteristics

###number of people with less than high school certificate
less.than.high.school <- cbind.data.frame(neigh.codes, getData(census.tmp, "No certificate, diploma or degree", "less.than.high.school"))  
agg.2016 <- join(agg.2016, less.than.high.school[, -which(names(less.than.high.school) %in% c("Neighbourhood"))], by = "Hood_ID")

###percent of people with less than high school certificate
less.than.high.school.per <- cbind.data.frame(neigh.codes, 
                                              getData(census.tmp, "No certificate, diploma or degree", "less.than.high.school.per") / getData(census.tmp, "Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data") * 100)
agg.2016 <- join(agg.2016, less.than.high.school.per[, -which(names(less.than.high.school.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###number of people with high school or equivalent certificate
high.school.cert <- cbind.data.frame(neigh.codes, getData(census.tmp, "Secondary (high) school diploma or equivalency certificate", "high.school.cert"))  
agg.2016 <- join(agg.2016, high.school.cert[, -which(names(high.school.cert) %in% c("Neighbourhood"))], by = "Hood_ID")

###percent of people with high school certificate
high.school.cert.per <- cbind.data.frame(neigh.codes,
                                         getData(census.tmp, "Secondary (high) school diploma or equivalency certificate", "high.school.cert.per") / getData(census.tmp, "Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data") * 100) 
agg.2016 <- join(agg.2016, high.school.cert.per[, -which(names(high.school.cert.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###number of people with post-secondary education or higher
post.sec.or.above <- cbind.data.frame(neigh.codes, getData(census.tmp, "Postsecondary certificate, diploma or degree", "post.sec.or.above"))
agg.2016 <- join(agg.2016, post.sec.or.above[, -which(names(post.sec.or.above) %in% c("Neighbourhood"))], by = "Hood_ID")

###percent of people with post-secondary education or higher
post.sec.or.above.per <- cbind.data.frame(neigh.codes, 
                                          getData(census.tmp, "Postsecondary certificate, diploma or degree", "post.sec.or.above.per") / getData(census.tmp, "Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data") * 100) 
agg.2016 <- join(agg.2016, post.sec.or.above.per[, -which(names(post.sec.or.above.per) %in% c("Neighbourhood"))], by = "Hood_ID")

###Employment Data-----
###Get data on unemployment level of each neighbourhood
census.tmp <- as.data.frame(census) #Create temp data frame for relabelling factors from census data frame
census.tmp <- census.tmp[census.tmp$Topic == "Labour force status", ] #subset census data by labour force status

###Get number of unemployed people in each neighbourhood
unemployed <- cbind.data.frame(neigh.codes, getData(census.tmp, "Unemployed", "unemployed"))
agg.2016 <- join(agg.2016, unemployed[, -which(names(unemployed) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get unemployment rate for each neighbourhood
unemployment.rate <- cbind.data.frame(neigh.codes, getData(census.tmp, "Unemployment rate", "unemployment.rate"))
agg.2016 <- join(agg.2016, unemployment.rate[, -which(names(unemployment.rate) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get number of unemployed males
unemployed.males <- cbind.data.frame(neigh.codes, getData(census.tmp, "Unemployed (Males)", "unemployed.males"))
agg.2016 <- join(agg.2016, unemployed.males[, -which(names(unemployed.males) %in% c("Neighbourhood"))], by = "Hood_ID")

###Get unemployment rate for males
unemployment.rate.males <- cbind.data.frame(neigh.codes, getData(census.tmp, "Unemployment rate (Males)", "unemployment.rate.males"))
agg.2016 <- join(agg.2016, unemployment.rate.males[, -which(names(unemployment.rate.males) %in% c("Neighbourhood"))], by = "Hood_ID")

###Merge crime data with 2016 agg data
agg.2016 <- as.data.frame(join(agg, agg.2016, by = "Hood_ID"))


