#install and load packages

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

### GETTING DATA ###

#data source: https://divvy-tripdata.s3.amazonaws.com/index.html

#list all files 

#rm(list=ls())
#dir("Data",full.names = T)

### NOTE: Zipped files for the datasets can be found in Archive folder ###

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


### CLEANING DATA ###

#remove any empty rows or columns in bike_rides data frame

bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))

#convert "started_at" and "ended_at" columns into date/time type

bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

#create date,month,day of the week, and year fields

bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

bike_rides$date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

bike_rides$month <- format(as.Date(bike_rides$date),"%m")
bike_rides$day <- format(as.Date(bike_rides$date),"%d")
bike_rides$year <- format(as.Date(bike_rides$date),"%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date),"%A")

  
#calculate trip duration (in seconds) 

bike_rides$ride_length_seconds <- difftime(bike_rides$ended_at,bike_rides$started_at)

#change data type of ride_length_seconds into numeric type

bike_rides$ride_length_seconds <- as.numeric(as.character(bike_rides$ride_length_seconds))

#remove irrelevant data where bikes were being checked/ride length was negative

bike_rides_v2 <- bike_rides[!(bike_rides$start_station_name == "HQ QR" | bike_rides$ride_length_seconds<0 | bike_rides$ride_length_seconds>604800),]

#change the order of days of the week to be correct

bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#this is now our cleaned dataset

### ANALYSIS ###

#check mean, median, max, min of ride_length_seconds

summary(bike_rides_v2$ride_length_seconds)

#compare above measures between members and casual users

setNames(aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = mean),c("Membership Type","Mean Ride Length (seconds)"))
setNames(aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = median),c("Membership Type","Median Ride Length (seconds)"))
setNames(aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = max),c("Membership Type","Max Ride Length (seconds)"))
setNames(aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = min),c("Membership Type","Min Ride Length (seconds)"))

#find average ride time by each day for members vs. casual

setNames(aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual + bike_rides_v2$day_of_week, FUN = mean),c("Membership Type","Weekday","Avg. Daily Ride (seconds)"))

#analyze ridership data by type and weekday

bike_rides_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual,weekday) %>%  #groups by user type and weekday
  summarize(number_of_rides = n(), average_duration = mean(ride_length_seconds)) %>%  #calculations
  arrange(weekday)  

#analyze how many trips there are each day with each type of bike for members vs. casuals

bike_rides_v2 %>% 
  count(member_casual,day_of_week, rideable_type)

#check how the times that casuals/members start trips are different

bike_rides_v2 %>% 
  count(member_casual,start_hour)

### VISUALIZATIONS ###


#plot number of rides started at each hour of the day

bike_rides_v2 %>% 
  count(member_casual,start_hour) %>% 
  ggplot(aes(x=start_hour,y=n,group=member_casual,color=member_casual)) +
  geom_line(size=1.3) +
  labs(title="Number of Rides Started Every Hour", x="Starting Hour", y="Number of Rides") +
  scale_y_continuous(labels = comma) + scale_x_continuous(n.breaks = 22) +
  scale_color_discrete(name="Rider Type")       #change legend title

  ggsave("./Visualizations/rides_every_hour.png")
  
#visualize average ride duration for members vs. casuals

bike_rides_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual,weekday) %>%  #groups by user type and weekday
  summarize(number_of_rides = n(), average_duration = (mean(ride_length_seconds)/60)) %>%  #calculations (change seconds to minutes to make it easier to visualize)
  arrange(member_casual,weekday) %>% 
  
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual)) +
  geom_col(position="dodge") +
  labs(title="Average Duration of Bike Rides per Day",x="Day of the Week",y="Average Duration (Minutes)") +
  labs(fill="Rider Type")  

  ggsave("./Visualizations/avg_duration_per_day.png")


#visualize number of rides by rider type

bike_rides_v2 %>% 
  ggplot() + geom_bar(mapping=aes(x=day_of_week,fill=rideable_type)) +
  facet_wrap(~member_casual) +
  labs(title="Number of Rides by Rider Type",x="Day of the Week",y="Number of Rides") +
  scale_y_continuous(labels = comma) +     
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(fill="Type of Bike")           
  
  ggsave("./Visualizations/number_of_rides_by_rider_type.png")  
  
  
  