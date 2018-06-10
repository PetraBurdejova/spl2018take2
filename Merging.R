source("Install_Packages.R")

###load and filter datasets----
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

####Aggregate crimes by type----
a.dt <- as.data.table(a.dt)
setkey(a.dt, "MCI", "Hood_ID")
agg <- a.dt[, .(count = .N), by = c("MCI", "Hood_ID")]
agg <- dcast(agg, Hood_ID ~ MCI)
agg$Hood_ID <- as.factor(agg$Hood_ID)

#Turn Nas to 0 for crimes that didn't occur in that neighbourhood
agg[is.na(agg)] <- 0


###Add Drug Arrests to aggregate dataset----
drugs$Neighbourhood.Id <- as.factor(drugs$Neighbourhood.Id)
agg <- merge(agg, drugs[,c("Neighbourhood.Id", "Drug.Arrests")], by.x = "Hood_ID", by.y = "Neighbourhood.Id" )

###GEt crime totals for each neighbourhood
agg$Total.crime <- rowSums(agg[,-1])

##Merge agg with wbt to get crime and neighbourhood profiles in 1 data frame
wbt$Neighbourhood.Id <- as.factor(wbt$Neighbourhood.Id)
agg.2014 <- merge(agg, wbt, by.x = "Hood_ID", by.y = "Neighbourhood.Id")
agg.2014[,c("Neighbourhood", "Combined.Indicators")] <- NULL

###Clean 2016 neighbourhood profile dataset to get variables---- 
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

###Create function to get data from main dataset
getData <- function(x, characteristic) {
  a <- subset(x, Characteristic == characteristic) 
  a <-  as.data.frame(colSums(a[,-1]))
}

####Turn columns into numeric
df1[,-c(1,2)] <- lapply(df1[,-c(1,2)], as.numeric)
df1[,c("Topic", "City.of.Toronto")] <- NULL

###Create dataframe to aggregate all variables from 2016 neighbourhod profiles dataset
agg.2016 <- cbind.data.frame(Hood_ID = neigh.codes$Hood_ID)

###Age and Gender variables----
###Get number of males from 15 - 24 in 2016
df2 <- as.data.frame(df1)
df2 <- df2[c(14:55),]
df2$Characteristic <- fct_collapse(df2$Characteristic,
                         male.youth = c("Male: 15 to 19 years", "Male: 20 to 24 years")
                    )

male.youth <- getData(df2, "male.youth")
colnames(male.youth) <- c("male.youth")
agg.2016 <- cbind.data.frame(agg.2016, male.youth)

###Get number of youth (15 - 24) in 2016

df2 <- as.data.frame(df1)
df2 <- df2[c(14:55),]
df2$Characteristic <- fct_collapse(df2$Characteristic,
                                   youth = c("Male: 15 to 19 years", "Male: 20 to 24 years", "Female: 15 to 19 years", "Female: 20 to 24 years")
)

youth <- getData(df2, "youth")
colnames(youth) <- c("youth")
agg.2016 <- cbind.data.frame(agg.2016, youth)


####Get number of males in 2016
df2 <- as.data.frame(df1)
df2 <- df2[c(14:55),]
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
male.above.15 <- getData(df2, "male.above.15")
colnames(male.above.15) <- c("male.above.15")
agg.2016 <- cbind.data.frame(agg.2016, male.above.15)

#SUbset females above 15 years old
female.above.15 <- getData(df2, "female.above.15")
colnames(female.above.15) <- c("female.above.15")
agg.2016 <- cbind.data.frame(agg.2016, female.above.15)

#Get population for 2016
df2 <- as.data.frame(df1)

df2$Characteristic <- fct_collapse(df2$Characteristic,
                                   population.2016 = c("Population, 2016"))

population.2016 <- getData(df2, "population.2016")
colnames(population.2016) <- c("population.2016")
agg.2016 <- cbind.data.frame(agg.2016, population.2016)

###Get Lone Parent Families by sex of parent
df2 <- as.data.frame(df1)
df2 <- df2[c(88:94),]

###Total # of Lone parent families
lone.parent.families <- getData(df2, "Total lone-parent families by sex of parent")
colnames(lone.parent.families) <- "lone.parent.families"
agg.2016 <- cbind.data.frame(agg.2016, lone.parent.families)

###percent of lone parent families
lone.parent.families.perc <- getData(df2, "Total lone-parent families by sex of parent") / 
                                getData(df2, "Total number of census families in private households")

colnames(lone.parent.families.perc) <- "lone.parent.families.perc"
agg.2016 <- cbind.data.frame(agg.2016, lone.parent.families.perc)

###Get Income characteristices of each neighbourhood----
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
low.income <- getData(df2, "low.income")
colnames(low.income) <- c("low.income")
agg.2016 <- cbind.data.frame(agg.2016, low.income)

####GEt number of middle income people in each neighbourhood
middle.income <- getData(df2, "middle.income")
colnames(middle.income) <- c("middle.income")
agg.2016 <- cbind.data.frame(agg.2016, middle.income)

####GEt number of high income people in each neighbourhood
high.income <- getData(df2, "high.income")
colnames(high.income) <- c("high.income")
agg.2016 <- cbind.data.frame(agg.2016, high.income)

###Get average income
df2 <- as.data.frame(df1)
df2 <- df2[c(2261:2363),]

avg.income <- getData(df2, "Total income: Average amount ($)")
colnames(avg.income) <- "avg.income"
agg.2016 <- cbind.data.frame(agg.2016, avg.income)

###percent of people taking unemployment benefits (EI)
people.ei <- getData(df2, "Employment Insurance (EI) benefits: Population with an amount")
colnames(people.ei) <- "people.ei" 
agg.2016 <- cbind.data.frame(agg.2016, people.ei)

###percent of people taking unemployment benefits (EI)
people.ei.per <- getData(df2, "Employment Insurance (EI) benefits: Population with an amount") /
                    getData(df2, "Total income: Population with an amount")
colnames(people.ei.per) <- "people.ei.per"
agg.2016 <- cbind.data.frame(agg.2016, people.ei.per)

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
agg.2016 <- cbind.data.frame(agg.2016, median.income)

#####Calculate number of Households in bottom 20% of Income distribution
df2 <- as.data.frame(df1)
df2 <- df2[c(1106:1116),]
df2 <- df2[!df2$Characteristic == "In the top half of the distribution",]

##Calculate sum of households in bottom 20% of income distribution
no.hholds.bottom.20per <- as.data.frame(colSums(df2[df2$Characteristic == "In the bottom decile" | df2$Characteristic == "In the second decile", -1]))
colnames(no.hholds.bottom.20per) <- "no.hholds.bottom.20per" 
agg.2016 <- cbind.data.frame(agg.2016, no.hholds.bottom.20per)

##Calculate percentage of households in bottom 20% of income distribution
hholds.bottom.20per.per <- round(no.hholds.bottom.20per / colSums(df2[,-1]), 2)
colnames(hholds.bottom.20per.per) <- "hholds.bottom.20per.per"
agg.2016 <- cbind.data.frame(agg.2016, hholds.bottom.20per.per)

###Get number of low income individuals
df2 <- as.data.frame(df1)
df2 <- df2[c(1122:1131),]

#Total number of low income individuals according to low income measure
low.income.pop <- getData(df2, "In low income based on the Low-income measure, after tax (LIM-AT)")
colnames(low.income.pop) <- c("low.income.pop")
agg.2016 <- cbind.data.frame(agg.2016, low.income.pop)

###Number of low income individuals according to low income measure 18-64 years
low.income.pop.18.to.64 <- getData(df2, "18 to 64 years")
colnames(low.income.pop.18.to.64) <- c("low.income.pop.18.to.64")  
agg.2016 <- cbind.data.frame(agg.2016, low.income.pop.18.to.64)

###Perc of low income individuals according to low income measure
low.income.pop.perc <- getData(df2, "Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)")
colnames(low.income.pop.perc) <- c("low.income.pop.perc")  
agg.2016 <- cbind.data.frame(agg.2016, low.income.pop.perc)

###Perc of low income individuals according to low income measure 18-64 years
low.income.pop.perc.18.to.64 <- getData(df2, "18 to 64 years (%)")
colnames(low.income.pop.perc.18.to.64) <- c("low.income.pop.perc.18.to.64")  
agg.2016 <- cbind.data.frame(agg.2016, low.income.pop.perc.18.to.64)

###Citizenship and Immigration stats of residents----
###GEt number of non-Canadian citizens in each neighbourhood
df2 <- as.data.frame(df1)
df2 <- df2[c(1142:1146),]

####Get number of non-Canadian citizens in each neighborhood
non.citizens <- getData(df2, "Not Canadian citizens")
colnames(non.citizens) <- "non.citizens"
agg.2016 <- cbind.data.frame(agg.2016, non.citizens)

###Get percentage of non-citizens in neighbourhood
non.citizens.perc <- getData(df2, "Not Canadian citizens") / getData(df2, "Total - Citizenship for the population in private households - 25% sample data")
colnames(non.citizens.perc) <- "non.citizens.perc"
agg.2016 <- cbind.data.frame(agg.2016, non.citizens.perc)

###Get number of immigrants in each neighbourhood
df2 <- as.data.frame(df1)
df2 <- df2[c(1147:1156),]

###Get number of immigrants in each neighbourhood
immigrants <- getData(df2, "Immigrants")
colnames(immigrants) <- "immigrants"
agg.2016 <- cbind.data.frame(agg.2016, immigrants)

###Get percentage of immigrants in each neighbourhood
immigrants.perc <- getData(df2, "Immigrants") / getData(df2, "Total - Immigrant status and period of immigration for the population in private households - 25% sample data")
colnames(immigrants.perc) <- "immigrants.perc"
agg.2016 <- cbind.data.frame(agg.2016, immigrants.perc)

####Number of immigrants in last 5 years as recent immigrants
immigrants.recent <- getData(df2, "2011 to 2016")
colnames(immigrants.recent) <- "immigrants.recent"
agg.2016 <- cbind.data.frame(agg.2016, immigrants.recent)

###percentage of population that are recent immigrants
immigrants.recent.perc <- getData(df2, "2011 to 2016") / getData(df2, "Total - Immigrant status and period of immigration for the population in private households - 25% sample data")
colnames(immigrants.recent.perc) <- "immigrants.recent.perc"
agg.2016 <- cbind.data.frame(agg.2016, immigrants.recent.perc)

####Get number of refugees who landed from1986-2016 in each neighbourhood
df2 <- as.data.frame(df1)
df2 <- df2[c(1289:1295),]

####Get number of refugees who landed from 1986-2016 in each neighbourhood
refugees <- getData(df2, "Refugees")
colnames(refugees) <- "refugees"
agg.2016 <- cbind.data.frame(agg.2016, refugees)

####Get percent of refugees who landed from 1986-2016 in each neighbourhood
refugees.perc <- getData(df2, "Refugees") / population.2016
colnames(refugees.perc) <- "refugees.perc"
agg.2016 <- cbind.data.frame(agg.2016, refugees.perc)

####Get number of visible minorities in each neighbourhood
df2 <- as.data.frame(df1)
df2 <- df2[c(1330:1344),]

####Get number of visible minorities in each neighbourhood
vis.minorities <- getData(df2, "Total visible minority population")
colnames(vis.minorities) <- "vis.minorities"
agg.2016 <- cbind.data.frame(agg.2016, vis.minorities)

###Get percent of pop that are visible minorities
vis.minorities.perc <- getData(df2, "Total visible minority population") / getData(df2, "Total - Visible minority for the population in private households - 25% sample data")
colnames(vis.minorities.perc) <- "vis.minorities.perc"
agg.2016 <- cbind.data.frame(agg.2016, vis.minorities.perc)

###Housing characteristics----
###Get number of renters
df2 <- as.data.frame(df1)
df2 <- df2[c(1624:1626),]

renters <- getData(df2, "Renter")
colnames(renters) <- "renters"
agg.2016 <- cbind.data.frame(agg.2016, renters)

####Get percent of renters in each neighbourhood
renters.perc <- getData(df2, "Renter") / getData(df2, "Total - Private households by tenure - 25% sample data")
colnames(renters.perc) <- "renters.perc"
agg.2016 <- cbind.data.frame(agg.2016, renters.perc)

###Get number of dwellings that are not condominiums in each neighbourhood
df2 <- as.data.frame(df1)
df2 <- df2[c(1628:1630),]

houses <- getData(df2, "Not condominium")
colnames(houses) <- "houses"
agg.2016 <- cbind.data.frame(agg.2016, houses)

###Get percent of buildings that are not condominiums in each neighbourhood
houses.perc <- getData(df2, "Not condominium") / getData(df2, "Total - Occupied private dwellings by condominium status - 25% sample data")
colnames(houses.perc) <- "houses.perc"
agg.2016 <- cbind.data.frame(agg.2016, houses.perc)

###Get number of households living in unsuitable housing conditions for size and makeup of family, 
###according to statscanada definition
df2 <- as.data.frame(df1)
df2 <- df2[c(1646:1648),]

unsuitable.housing <- getData(df2, "Not suitable")
colnames(unsuitable.housing) <- "unsuitable.housing"
agg.2016 <- cbind.data.frame(agg.2016, unsuitable.housing)

###percent hholds in unsuitable housing
unsuitable.housing.perc <- getData(df2, "Not suitable") / getData(df2, "Total - Private households by housing suitability - 25% sample data")
colnames(unsuitable.housing.perc) <- "unsuitable.housing.perc"
agg.2016 <- cbind.data.frame(agg.2016, unsuitable.housing.perc)

###Get households that require major repairs
df2 <- as.data.frame(df1)
df2 <- df2[c(1657:1659),]

hhlds.mjr.rprs <- getData(df2, "Major repairs needed")
colnames(hhlds.mjr.rprs) <- "hhlds.mjr.rprs"
agg.2016 <- cbind.data.frame(agg.2016, hhlds.mjr.rprs)

###Get percent or hhlds that need major repairs
hhlds.mjr.rprs.perc <- getData(df2, "Major repairs needed") / getData(df2, "Total - Occupied private dwellings by dwelling condition - 25% sample data")
colnames(hhlds.mjr.rprs.perc) <- "hhlds.mjr.rprs.perc"
agg.2016 <- cbind.data.frame(agg.2016, hhlds.mjr.rprs.perc)

###Get households that spend 30 percent or more of income on shelter costs
df2 <- as.data.frame(df1)
df2 <- df2[c(1673:1676),]

more.than.30per.on.shltr <- getData(df2, "Spending 30% or more of income on shelter costs")
colnames(more.than.30per.on.shltr) <- "unaffordable.housing"
agg.2016 <- cbind.data.frame(agg.2016, more.than.30per.on.shltr)

###Get percent of households that spend more than 30 percent of income on shelter costs
more.than.30per.on.shltr.perc <- getData(df2, "Spending 30% or more of income on shelter costs") /
                                    getData(df2, "Total - Owner and tenant households with household total income greater than zero; in non-farm; non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data")

colnames(more.than.30per.on.shltr.perc) <- "unaffordale.housing.perc"
agg.2016 <- cbind.data.frame(agg.2016, more.than.30per.on.shltr.perc)

###Education of residents-----
###Get data on education level of residents of each neighbourhood

df2 <- as.data.frame(df1)
df2 <- df2[c(1698:1701),]

###number of people with less than high school certificate

less.than.high.school <- getData(df2, "No certificate, diploma or degree")  
colnames(less.than.high.school) <- "less.than.high.school" 
agg.2016 <- cbind.data.frame(agg.2016, less.than.high.school)

###percent of people with less than high school certificate

less.than.high.school.perc <- getData(df2, "No certificate, diploma or degree") / getData(df2, "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data") 
colnames(less.than.high.school.perc) <- "less.than.high.school.perc"
agg.2016 <- cbind.data.frame(agg.2016, less.than.high.school.perc)

###number of people with high school or equivalent certificate

high.school.cert <- getData(df2, "Secondary (high) school diploma or equivalency certificate")  
colnames(high.school.cert) <- "high.school.cert"
agg.2016 <- cbind.data.frame(agg.2016, high.school.cert)

###percent of people with less than high school certificate

high.school.cert.perc <- getData(df2, "Secondary (high) school diploma or equivalency certificate") / getData(df2, "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data") 
colnames(high.school.cert.perc) <- "high.school.cert.perc"
agg.2016 <- cbind.data.frame(agg.2016, high.school.cert.perc)

###number of people with post-secondary education or higher
post.sec.or.above <- getData(df2, "Postsecondary certificate, diploma or degree")
colnames(post.sec.or.above) <- "post.sec.or.above"
agg.2016 <- cbind.data.frame(agg.2016, post.sec.or.above)

###percent of people with post-secondary education or higher
post.sec.or.above.perc <- getData(df2, "Postsecondary certificate, diploma or degree") / getData(df2, "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data") 
colnames(post.sec.or.above.perc) <- "post.sec.or.above.perc"
agg.2016 <- cbind.data.frame(agg.2016, post.sec.or.above.perc)

###Employment Data-----
###Get data on unemployment level of each neighbourhood

df2 <- as.data.frame(df1)
df2 <- df2[c(1880:1895),]

###Get number of unemployed people in each neighbourhood
unemployed <- getData(df2, "Unemployed")
colnames(unemployed) <- "unemployed"
agg.2016 <- cbind.data.frame(agg.2016, unemployed)

###Get unemployment rate for each neighbourhood
unemployment.rate <- getData(df2, "Unemployment rate")
colnames(unemployment.rate) <- "unemployment.rate"
agg.2016 <- cbind.data.frame(agg.2016, unemployment.rate)

###Get number of unemployed males
unemployed.males <- getData(df2, "Unemployed (Males)")
colnames(unemployed.males) <- "unemployed.males"
agg.2016 <- cbind.data.frame(agg.2016, unemployed.males)

###Get unemployment rate for males
unemployment.rate.males <- getData(df2, "Unemployment rate (Males)")
colnames(unemployment.rate.males) <- "unemployment.rate.males"
agg.2016 <- cbind.data.frame(agg.2016, unemployment.rate.males)

###Mergecrime data with 2016 agg data
agg.2016 <- merge(agg, agg.2016, by.x = "Hood_ID", by.y = "Hood_ID" )

###TESTING




