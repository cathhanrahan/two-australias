rm(list = ls())

library(dplyr)
library(tidyr)
library(reshape)
library(stringi)
library(fuzzyjoin)

#TAX DATA

#Read in 2013-14 raw tax data
rawTaxData201314 <- read.csv("201314rawTaxData.csv", stringsAsFactors = FALSE)

#Remove commas from numbers and convert to numbers
rawTaxData201314$Total.Income.or.Loss3.no. <- as.numeric(gsub(",", "", rawTaxData201314$Total.Income.or.Loss3.no.))
rawTaxData201314$Total.Income.or.Loss3.. <- as.numeric(gsub(",", "", rawTaxData201314$Total.Income.or.Loss3..))

#Change column names to taxStatus, state, postcode, totalIncomeNumber, totalIncomeValue
names(rawTaxData201314) <- c("taxStatus", "state", "postcode", "totalIncomeNumber14", "totalIncomeValue14")

#Add zero to front of three digit postcodes
rawTaxData201314$postcode <- stri_pad(rawTaxData201314$postcode, width = 4, pad = "0")

#Group by postcode, then sum the taxable and non-taxable lines for each postcode
summedTaxData201314 <- rawTaxData201314 %>% group_by(postcode, state) %>% summarise(overallNo14 = sum(totalIncomeNumber14), overallValue14 = sum(totalIncomeValue14))

#Read in 2010-11 raw tax data
rawTaxData201011 <- read.csv("201011rawTaxData.csv", stringsAsFactors = FALSE)

#Remove commas from numbers and convert to numbers
rawTaxData201011$Total.income.number <- as.numeric(gsub(",", "", rawTaxData201011$Total.income.number))
rawTaxData201011$Total.Income.or.Loss <- as.numeric(gsub(",", "", rawTaxData201011$Total.Income.or.Loss))

#Change column names
names(rawTaxData201011) <- c("postcode", "state", "totalIncomeNumber11", "totalIncomeValue11")

#Add zero to front of three digit postcodes
rawTaxData201011$postcode <- stri_pad(rawTaxData201011$postcode, width = 4, pad = "0")


#LGA TO POSTCODE DATA

#Read in LGA to postcodes data for 2011
lgaToPostcodes2011 <- read.csv("lgaToPostcodes2011.csv", stringsAsFactors = FALSE)

#Add zero to front of three digit postcodes in LGA 2011 data
lgaToPostcodes2011$postcode <- stri_pad(lgaToPostcodes2011$postcode, width = 4, pad = "0")

#Read in LGA postcodes data for 2014
lgaToPostcodes2014 <- read.csv("lgaToPostcodes2014.csv", stringsAsFactors = FALSE)

#Add zero to front of three digit postcodes in LGA 2014 data
lgaToPostcodes2014$postcode <- stri_pad(lgaToPostcodes2014$postcode, width = 4, pad = "0")

#Delete second header row
lgaToPostcodes2011 <- lgaToPostcodes2011[-c(1859), ]

#Convert Extra file columns to factor in 2011 file (code and ratio)
lgaToPostcodes2011$lgaCode <- as.integer(lgaToPostcodes2011$lgaCode)   
lgaToPostcodes2011$Ratio <- as.numeric(lgaToPostcodes2011$Ratio)

#COMBINING 2011 TAX DATA AND LGA DATA

#Combine LGA 2014 postcode data with tax data 2011 using a left join 
taxLgaCoded2011 <- left_join(rawTaxData201011, lgaToPostcodes2014, by = "postcode") 

#Delete row with "other" tax
taxLgaCoded2011 <- taxLgaCoded2011[-c(4044), ]

#Calculate number of filers and total value for LGAs by multiplying ratio
taxLgaCoded2011 <- mutate(taxLgaCoded2011, lganumber11 = totalIncomeNumber11 * Ratio, lgavalue11 = totalIncomeValue11 * Ratio)

#sHOW AVERAGE TOTAL INCOME 2011 BY LGA
taxLgaCoded2011Ave <- taxLgaCoded2011 %>%
  group_by(lgaCode, lgaName) %>%
  summarise(AveIncome11 = (sum(lgavalue11)/sum(lganumber11)))

#Inflate average 2011 income by CPI 7.6%
taxLgaCoded2011Ave <- mutate(taxLgaCoded2011Ave,
                             inflatedAveInc11 = (AveIncome11*0.076) + AveIncome11)

#COMBINING 2014 TAX DATA AND LGA DATA

#Combine LGA postcode data 2014 with tax data 2013-14 using a left join 
taxLgaCoded2014 <- left_join(summedTaxData201314, lgaToPostcodes2014, by = "postcode")

#Calculate number of filers and total value for LGAs by multiplying ratio (2013)
taxLgaCoded2014 <- mutate(taxLgaCoded2014, lganumber14 =
                            overallNo14 * Ratio, lgavalue14 = overallValue14 * Ratio)

#sHOW AVERAGE TOTAL INCOME BY LGA
taxLgaCoded2014ave <- taxLgaCoded2014 %>%
  group_by(lgaCode, lgaName) %>%
  summarise(AveIncome14 = (sum(lgavalue14)/sum(lganumber14)))

#COmbine 2011 and 2014 LGA average income files using full join to retain all values
combinedAveIncome <- full_join(taxLgaCoded2011Ave, taxLgaCoded2014ave, by = "lgaCode")

combinedAveIncome$lgaName.y <- NULL

#Calculate change in average income
combinedAveIncome <- mutate(combinedAveIncome, incomeChange = (AveIncome14 - inflatedAveInc11) / inflatedAveInc11 * 100)

#UNEMPLOYMENT

#Read in 2014 to 2016 LGA file
lga2014To2016 <- read.csv("lga2014To2016.csv", stringsAsFactors = FALSE)

#Read in csv of LGA unemployment numbers and labor force
unemplNo <- read.csv("unemplNo.csv", stringsAsFactors = FALSE)
labForce <- read.csv("labForce.csv", stringsAsFactors = FALSE)

#Remove commas and convert to numbers for unemplNo and labForce
unemplNo$Dec.10 <- as.numeric(gsub(",", "", unemplNo$Dec.10))
unemplNo$Dec.15 <- as.numeric(gsub(",", "", unemplNo$Dec.15))
labForce$Dec.10 <- as.numeric(gsub(",", "", labForce$Dec.10))
labForce$Dec.15 <- as.numeric(gsub(",", "", labForce$Dec.15))

#Combine unemplNo and labForce data and delete extra name column
unemplRawData <- full_join(unemplNo, labForce, by = "lgaCode")
unemplRawData$lgaName.y <- NULL

#Change unemployed raw data column names
names(unemplRawData) <- c("lgaName2016", "lgaCode2016", "dec10Unempl", "dec15Unempl", "dec10labForce", "dec15labForce")

#Combine LGA 2014-2016 postcode data with tax data 2011 using a left join 
unemplDataLgaCoded <- left_join(lga2014To2016, unemplRawData, by = "lgaCode2016") 

#Delete extra lga name column
unemplDataLgaCoded$lgaName2016.y <- NULL

#Calculate number of unemployed and labour force for LGAs by multiplying ratio 
unemplDataLgaCoded <- unemplDataLgaCoded %>%  
  mutate(lgadec10Unempl = dec10Unempl * ratio,
         lgadec15Unempl = dec15Unempl * ratio,
         lgalabForceDec10 = dec10labForce * ratio,
         lgalabForceDec15 = dec15labForce * ratio)

#sHOW UNEMPLOYMENT CHANGE BY 2014 LGA
unemplData2014lgaCoded <- unemplDataLgaCoded %>%
  group_by(lgaCode2014, lgaName2014) %>%
  summarise(unemplRateDec10 = sum(lgadec10Unempl)/sum(lgalabForceDec10)*100,
unemplRateDec15 = sum(lgadec15Unempl)/sum(lgalabForceDec15)*100)

#Calculate change in unemployment by 2014 LGA
unemplData2014lgaCoded <- mutate(unemplData2014lgaCoded,
                                 unemplChange = unemplRateDec15 - unemplRateDec10)

#Remove first two rows
unemplData2014lgaCoded <- unemplData2014lgaCoded[-c(1, 2), ]

#Change column names to match other datasets
names(unemplData2014lgaCoded) [1]  <- "lgaCode"
names(unemplData2014lgaCoded) [2] <- "lgaName"

#Change lgaCode to integer
unemplData2014lgaCoded$lgaCode <- as.integer(unemplData2014lgaCoded$lgaCode)


#GROSS REGIONAL PRODUCT DATA

#Read in gross regional product data 2011
grpData <- read.csv("grpData.csv", colClasses = c("grp2011" = "numeric", "grp2015" = "numeric"))

#Delete Maralinga line (no data)
grpData <- grpData[-c(309), ]

#Calculate share of total GRP for each LGA
grpData <- mutate(grpData, sharegrp = (grp2015 - grp2011)/(1568510 - 1401936)*100)


#LONG TERM UNEMPLOYED DATA


#Read in csv of long term unemployed data - Newstart number
lgaLongTermUnemplNewstartNo <- read.csv("lgaLongTermUnemplNewstartNo.csv", stringsAsFactors = TRUE)

#Change names of Long term unemployed data
names(lgaLongTermUnemplNewstartNo) <- c("lgaCode", "lgaName", "year", "newstartNo")

#Convert value columns to numbers in LTU Newstart number data
lgaLongTermUnemplNewstartNo$newstartNo <- as.numeric(as.character(lgaLongTermUnemplNewstartNo$newstartNo))

#Read in csv of long term unemployed data - LTU rate
lgaLongTermUnemplLTURate  <- read.csv("lgaLongTermUnemplLTURate.csv",stringsAsFactors = TRUE)

#Change names of Long term unemployed data
names(lgaLongTermUnemplLTURate) <- c("lgaCode", "lgaName", "year", "newstartRate")

#Convert value columns to numbers in LTU Newstart rate data
lgaLongTermUnemplLTURate$newstartRate <- as.numeric(as.character(lgaLongTermUnemplLTURate$newstartRate))

#Spread LTU Newstart number data into year columns
lgaLongTermUnemplNewstartNo <- spread(lgaLongTermUnemplNewstartNo, year, newstartNo)

#Rename year columns
names(lgaLongTermUnemplNewstartNo) <- c("lgaCode", "lganame", "No2010", "No2011", "No2012", "No2013", "No2014")

#Spread LTU Newstart rate data into year columns
lgaLongTermUnemplLTURate <- spread(lgaLongTermUnemplLTURate, year, newstartRate)

#Rename year columns
names(lgaLongTermUnemplLTURate) <- c("lgaCode", "lgaName", "Rate2010", "Rate2011", "Rate2012", "Rate2013", "Rate2014")

#Join LTU rate and number data together
combinedLtuData <- full_join(lgaLongTermUnemplNewstartNo, lgaLongTermUnemplLTURate, by = "lgaCode")

#Read in the Estimated Resident population data
erpData <- read.csv("erpData.csv")

#Change ERP data column names
names(erpData) <- c("lgaCode", "lgaName", "erp2010", "erp2011", "erp2012", "erp2013", "erp2014")

#Join LTU data with ERP data
combinedLtuData <- left_join(combinedLtuData, erpData, by = "lgaCode")

#Delete extra LGA name columns
combinedLtuData$lgaName.x <- NULL
combinedLtuData$lgaName.y <- NULL

#Calculate LTU per LG and then the change
combinedLtuData <- mutate(combinedLtuData, Ltu2010 = No2010 * Rate2010 / 100, 
                          LTuPercent2010 = Ltu2010 / erp2010 * 100,
                          Ltu2014 = No2014 * Rate2014 / 100, 
                          LTuPercent2014 = Ltu2014 / erp2014 * 100,
                          Change = LTuPercent2014 - LTuPercent2010)


#CREATE DATASET WITH ALL VARIABLES

fullDataSet <- full_join(combinedAveIncome, unemplData2014lgaCoded, by = "lgaCode")
fullDataSet <- full_join(fullDataSet, combinedLtuData, by = "lgaCode")
fullDataSet <- full_join(fullDataSet, grpData, by = "lgaCode")


write.csv(fullDataSet, file = "fullDataSet.csv")

#Preventable hospitalisations

#Read in LGA to SA3 concordance data
lgaToSA3 <- read.csv("lgaToSA3.csv", stringsAsFactors = TRUE)






# #OLd CODE
# 
# 
# 
# #UNEMPLOYMENT
# 
# #Read in csv of SA2 unemployment data
# sa2UnemploymentData <- read.csv("sa2UnemploymentData.csv")
# 
# #Convert SA2 code column in sa2 unemploymment data to factor
# sa2UnemploymentData$SA2.Code <- as.factor(sa2UnemploymentData$SA2.Code)
# 
# #Add employment rate change column (Sep.16 - Dec.10) to sa2 unemployment data
# sa2UnemploymentData <- mutate(sa2UnemploymentData, rateChange = Sep.16 - Dec.10)
# 
# #Read in csv of LGA unemployment data
# lgaUnemploymentData <- read.csv("lgaUnemploymentData.csv", stringsAsFactors = FALSE)
# 
# #Convert SA2 code column in lga unemploymment data to factor
# lgaUnemploymentData$LGA.Code <- as.factor(lgaUnemploymentData$LGA.Code)
# 
# #Add employment rate change column (Sep.16 - Dec.10) to lga unemployment data
# lgaUnemploymentData <- mutate(lgaUnemploymentData, rateChange = Sep.16 - Dec.10)
# 
# 
# #Change name of LGA and code columns to lgaName and lgaCode
# names(lgaUnemploymentData) [1] <- paste("lgaName")
# names(lgaUnemploymentData) [2] <- paste("lgaCode")
# 
# #Read in csv of LGA unemployment data
# lgaUnemploymentData <- read.csv("lgaUnemploymentData.csv", stringsAsFactors = FALSE)
# 
# #Add employment rate change column (Sep.16 - Dec.10) to lga unemployment data
# lgaUnemploymentData <- mutate(lgaUnemploymentData, rateChange = Sep.16 - Dec.10)
# 
# #LONG TERM UNEMPLOYED
# 
# #Read in csv of long term unemployed data
# lgaLongTermUnempl <- read.csv("lgaLongTermUnempl.csv", stringsAsFactors = TRUE)
# 
# #Change names of Long term unemployed data
# names(lgaLongTermUnempl) <- c("lgaCode", "lgaName", "year", "newstartNo", "ltuRate")
# 
# #Convert value columns to numbers
# lgaLongTermUnempl$newstartNo <- as.numeric(as.character(lgaLongTermUnempl$newstartNo))
# lgaLongTermUnempl$ltuRate <- as.numeric(as.character(lgaLongTermUnempl$ltuRate))
# 
# #Melt LGA LTU data by combining the number and value columns
# lgaLongTermUnempl <- melt(lgaLongTermUnempl, id.vars = c("lgaCode", "lgaName", "year"), measure.vars = c("newstartNo", "ltuRate"))
# 
# #Melt LGA LTU data by combining the number and value columns
# lgaLongTermUnempl <- melt(lgaLongTermUnempl, 
#                           id.vars = c("lgaCode", "lgaName", "year"),
#                           measure.vars = c("newstartNo", "ltuRate"))
# 
# #Spread LGA LTU into year columns
# lgaLongTermUnempl <- spread(lgaLongTermUnempl, year, value)
# 
# 
# #Cast the LGA LTU data to separate years
# 
# #TAX DATA
# 
# #Read in 2013-14 raw tax data
# rawTaxData201314 <- read.csv("201314rawTaxData.csv", stringsAsFactors = FALSE)
# 
# #Remove commas from numbers and convert to numbers
# rawTaxData201314$Total.Income.or.Loss3.no. <- as.numeric(gsub(",", "", rawTaxData201314$Total.Income.or.Loss3.no.))
# rawTaxData201314$Total.Income.or.Loss3.. <- as.numeric(gsub(",", "", rawTaxData201314$Total.Income.or.Loss3..))
# 
# #Change column names to taxStatus, state, postcode, totalIncomeNumber, totalIncomeValue
# names(rawTaxData201314) <- c("taxStatus", "state", "postcode", "totalIncomeNumber", "totalIncomeValue")
# 
# #Add zero to front of three digit postcodes
# rawTaxData201314$postcode <- stri_pad(rawTaxData201314$postcode, width = 4, pad = "0")
# 
# #Change taxStatus, postcode and state to characters
# rawTaxData201314$taxStatus <- as.character(rawTaxData201314$taxStatus)
# rawTaxData201314$state <- as.character(rawTaxData201314$state)
# rawTaxData201314$postcode <- as.character(rawTaxData201314$postcode)
# 
# #Change column names
# names(rawTaxData201314) <- c("taxStatus", "state", "postcode", "totalIncomeNumber", "totalIncomeValue")
# 
# #Add zero to front of three digit postcodes
# rawTaxData201314$postcode <- stri_pad(rawTaxData201314$postcode, width = 4, pad = "0")
# 
# #Group by postcode, then sum the taxable and non-taxable lines for each postcode
# summedTaxData201314 <- rawTaxData201314 %>% group_by(postcode, state) %>% summarise(overallNo = sum(totalIncomeNumber), overallValue = sum(totalIncomeValue))
# 
# 
# #Read in 2010-11 raw tax data
# rawTaxData201011 <- read.csv("201011rawTaxData.csv")
# 
# #Remove commas from numbers and convert to numbers
# rawTaxData201011$Total.Income.or.Loss <- as.numeric(gsub(",", "", rawTaxData201011$Total.Income.or.Loss))
# rawTaxData201011$X <- as.numeric(gsub(",", "", rawTaxData201011$X))
# 
# #Change postcode to characters
# rawTaxData201011$Postcode <- as.character(rawTaxData201011$Postcode)
# 
# #Change column names
# names(rawTaxData201011) <- c("postcode", "state", "totalIncomeNumber", "totalIncomeValue")
# 
# #Add zero to front of three digit postcodes
# rawTaxData201011$postcode <- stri_pad(rawTaxData201011$postcode, width = 4, pad = "0")
# 
# 
# #LGA TO POSTCODE DATA
# 
# #Read in LGA to postcodes data for 2011
# lgaToPostcodes2011 <- read.csv("lgaToPostcodes2011.csv", stringsAsFactors = FALSE)
# lgaToPostcodes2011$lgaCode <- as.numeric(lgaToPostcodes2011$lgaCode)
# 
# #Add zero to front of three digit postcodes in LGA 2011 data
# lgaToPostcodes2011$postcode <- stri_pad(lgaToPostcodes2011$postcode, width = 4, pad = "0")
# 
# #Read in LGA postcodes data for 2014
# lgaToPostcodes2014 <- read.csv("lgaToPostcodes2014.csv", stringsAsFactors = FALSE)
# 
# #Add zero to front of three digit postcodes in LGA 2014 data
# lgaToPostcodes2014$postcode <- stri_pad(lgaToPostcodes2014$postcode, width = 4, pad = "0")
# 
# #Create LGA data with any postcodes missing from 2011 data using 2014
# lgaToPostcodesExtra <- anti_join(lgaToPostcodes2014, lgaToPostcodes2011, by = "postcode")
# 
# #Convert Extra file columns to factor in 2011 file (code, name and ratio)
# lgaToPostcodes2011$lgaCode <- as.integer(lgaToPostcodes2011$lgaCode)   
# lgaToPostcodes2011$Ratio <- as.numeric(lgaToPostcodes2011$Ratio)
# 
# #Stack 2011 LGA codes to extra codes
# lgaToPostcodesCombined <- rbind(lgaToPostcodes2011, lgaToPostcodesExtra)
# 
# #Remove row with column names of second dataframe
# lgaToPostcodesCombined <- lgaToPostcodesCombined[-c(1859), ]
# 
# #Create LGA data with any postcodes missing from 2014 data using 2011
# lgaToPostcodesExtra2 <- anti_join(lgaToPostcodes2011, lgaToPostcodes2014, by = "postcode")
# 
# #Remove title row from extras@ DF
# lgaToPostcodesExtra2 <- lgaToPostcodesExtra2[-c(3), ]
# 
# #Stack 2011 LGA codes to extra codes
# lgaToPostcodesCombined2 <- rbind(lgaToPostcodes2014, lgaToPostcodesExtra2)
# 
# #COMBINING 2011 TAX DATA AND LGA DATA
# 
# #Combine LGA postcode data (combined) with tax data 2011 using a left join 
# taxLgaCoded2011 <- left_join(rawTaxData201011, lgaToPostcodesCombined, by = "postcode") 
# 
# #Change Ratio to a number
# taxLgaCoded2011$Ratio <- as.numeric(as.character(taxLgaCoded2011$Ratio))
# 
# #Calculate number of filers and total value for LGAs by multiplying ratio
# taxLgaCoded2011 <- mutate(taxLgaCoded2011, lganumber = totalIncomeNumber * Ratio, lgavalue = totalIncomeValue * Ratio)
# 
# #Remove the last row with "other" as postcode
# taxLgaCoded2011 <- taxLgaCoded2011[-c(4036), ]
# 
# #Remove NA rows to check number and income totals
# taxLgaCoded2011Check <- na.omit(taxLgaCoded2011)
# 
# #sHOW AVERAGE TOTAL INCOME BY LGA
# taxLgaCoded2011Ave <- taxLgaCoded2011 %>% group_by(lgaCode, lgaName) %>% summarise(AveIncome = (sum(lgavalue)/sum(lganumber)))
# 
# #Inflate average 2011 income by CPI 7.6%
# taxLgaCoded2011Ave <- mutate(taxLgaCoded2011Ave, inflatedAveInc = (AveIncome*0.076) + AveIncome)
# 
# #Change column name of income columns
# names(taxLgaCoded2011Ave) <- c("lgaCode", "lgaName", "AveIncome11", "inflatedAveIn11")
# 
# write.csv(taxLgaCoded2011Ave, file = "taxLgaCoded2011Ave.csv")
# 
# #Remove the last row with "other" as postcode
# taxLgaCoded2011Ave <- taxLgaCoded2011Ave[-c(563), ]
# 
# #COMBINING 2014 TAX DATA AND LGA DATA
# 
# #Combine LGA postcode data 2014 with tax data 2013-14 using a left join 
# taxLgaCoded2013 <- left_join(summedTaxData201314, lgaToPostcodesCombined2, by = "postcode") 
# 
# #Calculate number of filers and total value for LGAs by multiplying ratio
# taxLgaCoded2013 <- mutate(taxLgaCoded2013, lganumber = overallNo * Ratio, lgavalue = overallValue * Ratio)
# 
# #sHOW AVERAGE TOTAL INCOME BY LGA
# taxLgaCoded2013ave <- taxLgaCoded2013 %>% group_by(lgaCode, lgaName) %>% summarise(AveIncome = (sum(lgavalue)/sum(lganumber)))
# 
# #Remove the last row with "other" as postcode
# taxLgaCoded2013ave <- taxLgaCoded2013ave[-c(565), ]
# 
# #Change column name of income columns
# names(taxLgaCoded2013ave) <- c("lgaCode", "lgaName", "AveIncome13")
# 
# #COmbine 2011 and 2013 LGA average income files using full join to retain all values
# combinedAveIncome <- full_join(taxLgaCoded2011Ave, taxLgaCoded2013ave, by = "lgaCode")
# 
# write.csv(combinedAveIncome, file = "combinedAveIncome.csv")
# 
# #Matching postcodes and LGas
# 
# #Match 2011 postcodes to 2014 postcodes - leaves only postcodes in 2011 that are not in 2014
# lgaMatchPostcode <- anti_join(lgaToPostcodes2011, lgaToPostcodes2014, by = "postcode")
# write.csv(lgaMatchPostcode, file = "lgaMatchPostcode.csv")
# 
# #Match 2011 and 2014 LGAs - leaves only LGAs in 2011 that are not in 2014
# lgaMatchLga <- anti_join(lgaToPostcodes2011, lgaToPostcodes2014, by = "lgaCode")
# 
# which(is.na(lgaToPostcodes2011), arr.ind = TRUE)
# 
# #Read in full LTU dataset and then calc long term total for each row, then cast out as years,
# #Then add the pop data
# 
# #Read LTU full data set
# lgaLongTermUnempl <- read.csv("lgaLongTermUnempl.csv", stringsAsFactors = TRUE)
# 
# #Change names of Long term unemployed data
# names(lgaLongTermUnempl) <- c("lgaCode", "lgaName", "year", "newstartNo", "ltuRate")
# 
# #Convert value columns to numbers
# lgaLongTermUnempl$newstartNo <- as.numeric(as.character(lgaLongTermUnempl$newstartNo))
# lgaLongTermUnempl$ltuRate <- as.numeric(as.character(lgaLongTermUnempl$ltuRate))
# 
# #Calculate number LTU for each row
# lgaLongTermUnempl <- mutate(lgaLongTermUnempl, noLtu = newstartNo*ltuRate/100)
# 
# #Spread out by year - NO, FIND ANOTHER WAY
# lgaLongTermUnempl <- spread(lgaLongTermUnempl, year, noLtu)
# 
# 
