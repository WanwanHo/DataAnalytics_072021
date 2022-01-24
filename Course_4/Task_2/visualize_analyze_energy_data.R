# Title: Visualize and Analyze Energy Data
# Last update: 2022.1
# File: visualize_analyze_energy_data.R
# Project name: Power Usage Analytics for Sub-meters

################
# Load packages
################

install.packages("RMySQL")
install.packages("DBI")
install.packages("dplyr")
install.packages("lubridate")
install.packages("plotly")

library(DBI)
library(RMySQL)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggfortify)

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
DF_submeter$week <- week(DF_submeter$DateTime)
DF_submeter$weekDay <- wday(DF_submeter$DateTime)
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

## Plot all of sub-meter 1
#plot(DF_submeter$sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(DF_submeter, year == 2008 & month == 1 & day == 9)

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(houseDay, (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency 
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the second week of 2008 - 2 hour frequency
houseWeek <- filter(DF_submeter, year == 2008 & week == 2 & 
                      (hour == 0 | hour == 2 | hour == 4 | hour == 6 | hour == 8 | hour == 10 | 
                         hour == 12 | hour == 14 | hour == 16 | hour == 18 | hour == 20 | hour == 22 ))
## Plot subset houseWeek
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption of second Week in 2008",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset year 2008 - 5 day frequency
houseYear <- filter(DF_submeter, year == 2008 & (day == 1 | day == 5 | day == 10 | day == 15 | day == 20 | day == 25))

## Plot subset houseWeek
plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear$sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYear$sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption in 2008",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(DF_submeter, weekDay == 2 & hour == 20 & minute == 1)
## Create TS object with SubMeter3
tsSM3 <- ts(house070809weekly$sub_metering_3, frequency=52, start=c(2007,1))
autoplot(tsSM3, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Subset to one observation per hour for 2007, 2008 and 2009
house070809hourly <- filter(DF_submeter, hour == 18 & minute == 20 )
## Create TS object with SubMeter1
tsSM1 <- ts(house070809hourly$sub_metering_1, frequency=365, start=c(2007,1))
autoplot(tsSM1, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")

## Subset to one observation per hour for 2007, 2008 and 2009
house070809hourly <- filter(DF_submeter, hour == 16 & minute == 20 )
## Create TS object with SubMeter2
tsSM2 <- ts(house070809hourly$sub_metering_2, frequency=365, start=c(2007,1))
autoplot(tsSM2, ts.colour = 'orange', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")

## Apply time series linear regression to the sub-meter 3 ts object
library(forecast)
fitSM3 <- tslm(tsSM3 ~ trend + season) 

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 1 forecast with confidence levels 80 and 90
fitSM1 <- tslm(tsSM1 ~ trend + season) 
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))
## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 80), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 2 forecast with confidence levels 80 and 90
fitSM2 <- tslm(tsSM2 ~ trend + season) 
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))
## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 80), ylab= "Watt-Hours", xlab="Time")

## use summary to obtain R2 and RMSE from the models
summary(fitSM3)
summary(fitSM1)
summary(fitSM2)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)
##          Length Class  Mode     
## x        157    ts     numeric  
## seasonal 157    ts     numeric  
## trend    157    ts     numeric  
## random   157    ts     numeric  
## figure    52    -none- numeric  
## type       1    -none- character

## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 1
summary(components070809SM1weekly)
##         Length Class  Mode     
##x        1091   ts     numeric  
##seasonal 1091   ts     numeric  
##trend    1091   ts     numeric  
##random   1091   ts     numeric  
##figure    365   -none- numeric  
##type        1   -none- character

## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2)
## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM2weekly)
##          Length Class  Mode     
## x        1090   ts     numeric  
## seasonal 1090   ts     numeric  
## trend    1090   ts     numeric  
## random   1090   ts     numeric  
## figure    365   -none- numeric  
## type        1   -none- character

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_Adjusted <- tsSM3 - components070809SM3weekly$seasonal
autoplot(tsSM3_Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

#####################################
## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_Adjusted <- tsSM1 - components070809SM1weekly$seasonal
autoplot(tsSM1_Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

####################################
## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_Adjusted <- tsSM2 - components070809SM2weekly$seasonal
autoplot(tsSM2_Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 70))

## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 70), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))
