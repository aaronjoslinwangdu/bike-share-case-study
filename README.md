# Introduction

For this case study I am going to be analyzing data from [Divvy,](https://www.divvybikes.com) a bike share program based in Chicago. Divvy has a fleet of nearly 6,000 bicycles which are geotracked and rode within a network of 692 stations throughout Chicago. 

## What is the point of this case study?

The questions that I am using to quide my study are:
1. How do annual members and casual riders use Divvy bikes differently?
2. Why would casual riders buy Divvy annual membership?

## The Data

The datasets I used are public and can be found in the [Archive folder](https://github.com/aaronjoslinwangdu/bike-share-case-study/tree/master/Archive), as well as [here](https://divvy-tripdata.s3.amazonaws.com/index.html). The data has been made available by Motivate International Inc. under this [license](https://www.divvybikes.com/data-license-agreement).

## Preparation and Cleaning

First, I installed and loaded all of the necessary packages for preparing and cleaning the data.

```
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
```

Then I imported all of the .csv files, added them to separate dataframes, and combined them into one dataframe called **bike_rides**.

```
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

```
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))
```


Since the **started_at** and **ended_at** columns represent dates, I converted them into the correct data type.

```
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)
```

















