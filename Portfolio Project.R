#install (if first time using this package) and load needed package
#(code if needed) install.packages("dplyr")
#(code if needed) install.packages("gginference")
library(dplyr)
library(gginference)

#Upload crime dataset
RawData <- read.csv(file=
"C:/Users/mcman/Documents/CSU/MIS500/Module 8/Crimes2001-present.csv",
header=TRUE, na.strings="0", stringsAsFactors=FALSE)

#Upload community dataset 
CommData <- read.csv(file=
"C:/Users/mcman/Documents/CSU/MIS500/Module 8/socioeconomic_indicators.csv",
header=TRUE, na.strings="0", stringsAsFactors=FALSE)

#drop columns not needed for analysis 
RawData2 <- RawData[-c(5,11:13,15:17,19:22)]
CommData2 <- CommData[-c(3:7,9)]

#change TRUE/FALSE to 0/1 (respectively) in arrest and domestic columns as numeric
RawData2[,8:9] <- sapply(RawData2[,8:9], as.numeric)

#filter merged dataset to the years 2008 - 2012
FilteredCrimeData <- filter(RawData2, between(RawData2$Year, 2008,2012))

#Check Community and Crime data column names for proper column 
#merge, rename column as needed
colnames(FilteredCrimeData)
colnames(CommData2)
NewCommData <- rename(CommData2, Community.Area = ï..Community.Area.Number)

#Merge community data and crime dataset to include income 
#per capita in the community where the crime was commited
IncomeCrimeData <- merge(FilteredCrimeData, NewCommData, by="Community.Area")

#Run summary statistic of IncomeCrimeDataset
summary(IncomeCrimeData)

#filter for Communities 24 and 71 into separate variables
Community24 <- filter(IncomeCrimeData, IncomeCrimeData$Community.Area==24)
Community71 <- filter(IncomeCrimeData, IncomeCrimeData$Community.Area==71)

#calculate T-test for difference in arrest means in communities
t.test(Community24$Arrest, Community71$Arrest, alternative="two.sided")

#calculate Chi-square test for arrest and per capita income relationship
chisq.test(IncomeCrimeData$Arrest, IncomeCrimeData$PER.CAPITA.INCOME)

#plot T-test statistic and normal distribution
ggttest(t.test(Community24$Arrest, Community71$Arrest, alternative="two.sided"))

