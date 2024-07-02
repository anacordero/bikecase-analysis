---
title: "Cyclistc_Case_Study"
author: "Ana Cordero"
date: "2024-05-21"
output:
  html_document:
    df_print: paged
  word_document: default
  pdf_document: default
---

# Uplading initial packages to be use in this analysis. 
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(readr)
install.packages("plotrix")
library(plotrix)

# Load Data
Bike_Trips_2019_Q1 <- read.csv("~/Desktop/Case Studie - Bike Share/Raw_Data_CSV/Divvy_Trips_2019_Q1.csv")
View(Bike_Trips_2019_Q1)
Bike_Trips_2019_Q2 <- read.csv("~/Desktop/Case Studie - Bike Share/Raw_Data_CSV/Divvy_Trips_2019_Q2.csv")
View(Bike_Trips_2019_Q2)
Bike_Trips_2019_Q3 <- read.csv("~/Desktop/Case Studie - Bike Share/Raw_Data_CSV/Divvy_Trips_2019_Q3.csv")
View(Bike_Trips_2019_Q3)
Bike_Trips_2019_Q4 <- read.csv("~/Desktop/Case Studie - Bike Share/Raw_Data_CSV/Divvy_Trips_2019_Q4.csv")
View(Bike_Trips_2019_Q4)

nrow(Bike_Trips_2019_Q1) ###Confirm quantities of entry prior to create a new data frame.
nrow(Bike_Trips_2019_Q2)
nrow(Bike_Trips_2019_Q3)
nrow(Bike_Trips_2019_Q4)

## Create a new data frame with all the tables.
Total_Bike_Rides<-bind_rows(Bike_Trips_2019_Q1,Bike_Trips_2019_Q2,Bike_Trips_2019_Q3,Bike_Trips_2019_Q4) 
View(Total_Bike_Rides)
nrow(Total_Bike_Rides) ###Confirming the row counts is the same as before. 
write_csv(Total_Bike_Rides,file = "Total_Bike_Rides")

str(Total_Bike_Rides)
head(Total_Bike_Rides)

# Cleaning and check data for errors

### Removes N/As
Total_Bike_Rides_V1 <- na.omit(Total_Bike_Rides) 
View(Total_Bike_Rides_V1)
nrow(Total_Bike_Rides_V1)
write_csv(Total_Bike_Rides_V1,file = "Total_Bike_Rides_V1")

### The trip_id is unique per trip, so we need to make sure there is no two trips with the same id.
Total_Bike_Rides_V2 <- Total_Bike_Rides_V1[!duplicated(Total_Bike_Rides_V1$trip_id),] 
View(Total_Bike_Rides_V2)
nrow(Total_Bike_Rides_V2)
### No duplicates

### Filtering out all trip length equal or less than 'o'
Total_Bike_Rides_V3 <- filter(Total_Bike_Rides_V2,tripduration >= 0)
nrow(Total_Bike_Rides_V3)
View(Total_Bike_Rides_V3)
### No trip under 0 length. So we can assume that Total_Bike_Rides_V1 remains. 
write_csv(Total_Bike_Rides_V3,file = "Total_Bike_Rides_V3")

## Ensure all dates have the same format. 
Total_Bike_Rides_V3$start_time <- as.POSIXct(Total_Bike_Rides_V3$start_time, tz = "%Y-%m-%d %H:%M:%OS")
Total_Bike_Rides_V3$end_time <- as.POSIXct(Total_Bike_Rides_V3$end_time, tz = "%Y-%m-%d %H:%M:%OS")
View(Total_Bike_Rides_V3)


# Analysis
## Riders Type 
rider_type <- table(rider_type = Total_Bike_Rides_V3$usertype)
View(rider_type)
slices <- c(348217,2931036)
lbls <- c("Customers", "Suscribers")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep = " ")
pie(slices, labels = lbls, main = "Type of Riders",col = rainbow(length(lbls)))

## Riders per gender
rider_gender <- table(rider_gender = Total_Bike_Rides_V3$gender)
View(rider_gender)
slices <- c(20457,857977,2400819)
lbls <- c("N/A", "Female", "Male")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep = " ")
pie(slices, labels = lbls, main = "Riders per Gender",col = rainbow(length(lbls)))

## Customer per gender per rider type
rider_gender_usertype <- table(rider_gender = Total_Bike_Rides_V3$gender, costumer_type = Total_Bike_Rides_V3$usertype)
View(rider_gender_usertype)
slices <- c(4037,131438,212742,16420,726539,2188077)
lbls <- c("N/A Customers", "Female Customers", "Male Customers","N/A Suscribers","Female Suscribers","Male Suscribers")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep = " ")
pie(slices, labels = lbls, main = "Customers per Gender per User Type",col = rainbow(length(lbls)))

## Rider type per age
### Calculate age
Total_Bike_Rides_V4 <- mutate(Total_Bike_Rides_V3, age = 2019 - birthyear) 
View(Total_Bike_Rides_V4)
### Saving new data frame
write_csv(Total_Bike_Rides_V4, file = "Total_Bike_Rides_V4") 

###S ummary riders age
Average_age <- Total_Bike_Rides_V4 %>% 
  group_by(gender, usertype) %>%
  summarize((mean(age)))
View(Average_age)

## Trips data per rider type

### Verify the data class per column
### Convert trip duration data from a character to a numeric format.
sapply(Total_Bike_Rides_V4,class) 
Total_Bike_Rides_V4$trip_duration <- gsub(",","",Total_Bike_Rides_V4$tripduration) 
Total_Bike_Rides_V4$trip_duration <- as.numeric(Total_Bike_Rides_V4$trip_duration)
View(Total_Bike_Rides_V4)

### Trip length - average per rider type.
Trip_length_usertype <- Total_Bike_Rides_V4 %>%
  group_by(usertype) %>%
  summarize(mean(trip_duration))
View(Trip_length_usertype) 

Total_Bike_Rides_V5 <- mutate(Total_Bike_Rides_V4, (as.Date(start_time))) %>%
  select(,-2) 
View(Total_Bike_Rides_V5)
Total_Bike_Rides_V5$month <- format(as.Date(Total_Bike_Rides_V5$`(as.Date(start_time))`), "%m") %>%
  as.numeric(Total_Bike_Rides_V5$month)
View(Total_Bike_Rides_V5)
str(Total_Bike_Rides_V5)

### Counting trips per months
Trips_permonth <- table(Trips_permonth = Total_Bike_Rides_V5$month, customer_type = Total_Bike_Rides_V5$usertype) 
View(Trips_permonth)

### Counting trips per days
### Extract days
Total_Bike_Rides_V5$days <- format(weekdays(Total_Bike_Rides_V5$`(as.Date(start_time))`, abbreviate = FALSE )) 
View(Total_Bike_Rides_V5)
Trips_perdays <- table(Trips_perdays = Total_Bike_Rides_V5$days, customer_type = Total_Bike_Rides_V5$usertype) 
View(Trips_perdays)
str(Total_Bike_Rides_V5)

### Counting trips per start station.
Start_Stations <- table(Start_Stations = Total_Bike_Rides_V5$from_station_name) 
View(Start_Stations)

### Counting trips per end station.
End_Stations <- table(End_Stations = Total_Bike_Rides_V5$to_station_name) 
View(End_Stations)


  
