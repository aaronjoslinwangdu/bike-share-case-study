#install and load packages

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

#data source: https://divvy-tripdata.s3.amazonaws.com/index.html

#list all files 

rm(list=ls())
dir("Data",full.names = T)

#make each .csv file into a data frame

df1 <- read.csv("./Data/202004-divvy-tripdata.csv")
df2 <- read.csv("./Data/202005-divvy-tripdata.csv")
df3 <- read.csv("./Data/202006-divvy-tripdata.csv")
df4 <- read.csv("./Data/202007-divvy-tripdata.csv")
df5 <- read.csv("./Data/202008-divvy-tripdata.csv")
df6 <- read.csv("./Data/202009-divvy-tripdata.csv")
df7 <- read.csv("./Data/202010-divvy-tripdata.csv")
df8 <- read.csv("./Data/202011-divvy-tripdata.csv")
df9 <- read.csv("./Data/202012-divvy-tripdata.csv")
df10 <- read.csv("./Data/202101-divvy-tripdata.csv")
df11 <- read.csv("./Data/202102-divvy-tripdata.csv")
df12 <- read.csv("./Data/202103-divvy-tripdata.csv")

#combine all 12 data frames into 1

bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

#remove any empty rows or columns in bike_rides data frame

bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))

#convert "started_at" and "ended_at" columns into date/time type

bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

#create hour and date fields

bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

bike_rides$start_date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

#plot bike rides per day

bike_rides %>% 
  count(start_date) %>% 
  ggplot() + geom_line(aes(x=start_date,y=n))

#plot number of rides started at each hour of the day

bike_rides %>% 
  count(start_hour) %>% 
  ggplot() + geom_line(aes(x=start_hour,y=n))

#calculate trip duration (in seconds)

bike_rides$ride_length_seconds <- difftime(bike_rides$ended_at,bike_rides$started_at)











