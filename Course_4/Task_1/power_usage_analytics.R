# Title: Power Usage Analytics for the sub-meters
# Last update: 2021.12
# File: power_usage_analytics.R
# Project name: Power Usage Analytics for Sub-meters

################
# Load packages
################

install.packages("RMySQL")
install.packages("DBI")
install.packages("dplyr")
install.packages("lubridate")

library(DBI)
library(RMySQL)
library("dplyr")
library(lubridate)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'yr_2006')

## Use asterisk to specify all attributes for download
#irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, sub_metering_1, sub_metering_2, sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, sub_metering_1, sub_metering_2, sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, sub_metering_1, sub_metering_2, sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, sub_metering_1, sub_metering_2, sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, sub_metering_1, sub_metering_2, sub_metering_3 FROM yr_2010")

## Evaluate the data
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

## Combine tables into one dataframe using dplyr
DF_submeter <- bind_rows(yr_2007, yr_2008, yr_2009)

## Evaluate the primary dataframe
str(DF_submeter)
summary(DF_submeter)
head(DF_submeter)
tail(DF_submeter)
anyNA(DF_submeter)
anyDuplicated(DF_submeter)

## Combine Date and Time attribute values in a new attribute column
DF_submeter <-cbind(DF_submeter,paste(DF_submeter$Date,DF_submeter$Time), 
                      stringsAsFactors=FALSE)
head(DF_submeter)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(DF_submeter)[6] <-"DateTime"
head(DF_submeter)

## Move the DateTime attribute within the dataset
DF_submeter <- DF_submeter[c(ncol(DF_submeter), 1:(ncol(DF_submeter)-1))]
head(DF_submeter)

## Convert DateTime from character to POSIXct 
DF_submeter$DateTime <- as.POSIXct(DF_submeter$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(DF_submeter$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(DF_submeter)

## Create "year" attribute with lubridate
DF_submeter$year <- year(DF_submeter$DateTime)
DF_submeter$month <- month(DF_submeter$DateTime)
DF_submeter$day <- day(DF_submeter$DateTime)
DF_submeter$hour <- hour(DF_submeter$DateTime)
DF_submeter$minute <- minute(DF_submeter$DateTime)
str(DF_submeter)
head(DF_submeter)
summary(DF_submeter)

#         sub_metering_1   sub_metering_2   sub_metering_3  
#Min.   : Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#1st Qu.: 1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000 
#Median : Median : 0.000   Median : 0.000   Median : 1.000
#Mean   : Mean   : 1.159   Mean   : 1.343   Mean   : 6.216
#3rd Qu.: 3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000 
#Max.   : Max.   :82.000   Max.   :78.000   Max.   :31.000  
