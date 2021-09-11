# Introduction

For this case study I am going to be analyzing data from [Divvy,](https://www.divvybikes.com) a bike share program based in Chicago. Divvy has a fleet of nearly 6,000 bicycles which are geotracked and rode within a network of 692 stations throughout Chicago. This case study was my Capstone project for the Google Data Analytics Professional Certificate, and was done entirely in RStudio.

## What is the point of this case study?

The questions that I am using to guide my study are:
1. How do annual members and casual riders use Divvy bikes differently?
2. Why would casual riders buy Divvy annual membership?

## The Data

The datasets I used are public and can be found in the [Archive folder](https://github.com/aaronjoslinwangdu/bike-share-case-study/tree/master/Archive), as well as [here](https://divvy-tripdata.s3.amazonaws.com/index.html). The data has been made available by Motivate International Inc. under this [license](https://www.divvybikes.com/data-license-agreement).

## Preparation and Cleaning

First, I installed and loaded all of the necessary packages for preparing and cleaning the data.

```{r install}
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
```

Then I imported all of the .csv files, added them to separate dataframes, and combined them into one dataframe called **bike_rides**.

```{r import}
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

bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
```

The first step I took to clean the data was removing any empty rows or columns from **bike_rides**.

```{r remove_empty}
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))
```

Since the **started_at** and **ended_at** columns represent dates, I converted them into the correct data type.

```{r convert_ymd_hms}
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)
```

Then I used the **started_at** and **ended_at** columns to create separate columns for the day of the week, starting hour, month, etc. that each trip occurred on.

```{r separate_time_cols}
bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

bike_rides$date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

bike_rides$month <- format(as.Date(bike_rides$date),"%m")
bike_rides$day <- format(as.Date(bike_rides$date),"%d")
bike_rides$year <- format(as.Date(bike_rides$date),"%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date),"%A")
```

Added a column representing the duration of each trip in seconds, then changed it to the correct data type.

```{r difftime}
bike_rides$ride_length_seconds <- difftime(bike_rides$ended_at,bike_rides$started_at)
bike_rides$ride_length_seconds <- as.numeric(as.character(bike_rides$ride_length_seconds))
```

It was specified by the source that certain data was faulty/irrelevant, so I removed that data as well as negative/extremely high values for **ride_length_seconds** and created a separate, final dataframe called **bike_rides_v2**.

```{r create_bike_rides_v2}
bike_rides_v2 <- bike_rides[!(bike_rides$start_station_name == "HQ QR" | bike_rides$ride_length_seconds<0 | bike_rides$ride_length_seconds>604800),]
```

Finally, I corrected the order of the days of the week so my visualizations would be more intuitive.

```{r order_days}
bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
```

Now, onto the analysis!

## Analysis

Check the mean, median, max, min of **ride_length_seconds**.

```{r summary}
summary(bike_rides_v2$ride_length_seconds)
```

Compare the above measures between members and casual users

```{r aggregate_mem_vs_cas}
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = mean)
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = median)
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = max)
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = min)
```

Find the average ride time by each day for members vs. casuals.

```{r ride_time_per_day_mem_vs_cas}
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual + bike_rides_v2$day_of_week, FUN = mean)
```

Here, we analyze ridership data by type and weekday

```{r weekday_type}
bike_rides_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual,weekday) %>%  #groups by user type and weekday
  summarize(number_of_rides = n(), average_duration = mean(ride_length_seconds)) %>%  #calculations
  arrange(weekday)  
```

Check how many trips there are each day with each type of bike for members vs. casuals.

```{r bike_type}
bike_rides_v2 %>% 
  count(member_casual,day_of_week, rideable_type)
```

Check how the times that casuals/members start trips are different.

```{r start_time_diffs}
bike_rides_v2 %>% 
  count(member_casual,start_hour)

```


## Visualizations

![avg_duration_per_day](https://github.com/aaronjoslinwangdu/bike-share-case-study/blob/master/Visualizations/avg_duration_per_day.png)
---
![number_of_rides_by_rider_type](https://github.com/aaronjoslinwangdu/bike-share-case-study/blob/master/Visualizations/number_of_rides_by_rider_type.png)
---
![rides_every_hour](https://github.com/aaronjoslinwangdu/bike-share-case-study/blob/master/Visualizations/rides_every_hour.png)
---

## Conclusion







