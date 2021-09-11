bikeshare-case-study-markdown
================
Aaron Joslin-Wangdu
9/11/2021

# Introduction

For this case study I am going to be analyzing data from
[Divvy,](https://www.divvybikes.com) a bike share program based in
Chicago. Divvy has a fleet of nearly 6,000 bicycles which are geotracked
and rode within a network of 692 stations throughout Chicago. This case
study was my Capstone project for the Google Data Analytics Professional
Certificate, and was done entirely in RStudio.

## What is the point of this case study?

The questions that I am using to guide my study are: 1. How do annual
members and casual riders use Divvy bikes differently? 2. Why would
casual riders buy Divvy annual membership?

## The Data

The datasets I used are public and can be found in the [Archive
folder](https://github.com/aaronjoslinwangdu/bike-share-case-study/tree/master/Archive),
as well as [here](https://divvy-tripdata.s3.amazonaws.com/index.html).
The data has been made available by Motivate International Inc. under
this [license](https://www.divvybikes.com/data-license-agreement).

## Preparation and Cleaning

First, I installed and loaded all of the necessary packages for
preparing and cleaning the data.

``` r
install.packages("tidyverse",repos = 'http://cran.us.r-project.org')
install.packages("janitor",repos = 'http://cran.us.r-project.org')
install.packages("lubridate",repos = 'http://cran.us.r-project.org')
```

    ## Warning: cannot remove prior installation of package 'lubridate'

    ## Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying C:
    ## \Users\arkansas\Documents\R\R-4.1.1\library\00LOCK\lubridate\libs\x64\lubridate.dll
    ## to C:
    ## \Users\arkansas\Documents\R\R-4.1.1\library\lubridate\libs\x64\lubridate.dll:
    ## Permission denied

    ## Warning: restored 'lubridate'

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

Then I imported all of the .csv files, added them to separate
dataframes, and combined them into one dataframe called **bike\_rides**.

``` r
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

The first step I took to clean the data was removing any empty rows or
columns from **bike\_rides**.

``` r
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))
```

Since the **started\_at** and **ended\_at** columns represent dates, I
converted them into the correct data type.

``` r
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)
```

Then I used the **started\_at** and **ended\_at** columns to create
separate columns for the day of the week, starting hour, month, etc.
that each trip occurred on.

``` r
bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

bike_rides$date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

bike_rides$month <- format(as.Date(bike_rides$date),"%m")
bike_rides$day <- format(as.Date(bike_rides$date),"%d")
bike_rides$year <- format(as.Date(bike_rides$date),"%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date),"%A")
```

Added a column representing the duration of each trip in seconds, then
changed it to the correct data type.

``` r
bike_rides$ride_length_seconds <- difftime(bike_rides$ended_at,bike_rides$started_at)
bike_rides$ride_length_seconds <- as.numeric(as.character(bike_rides$ride_length_seconds))
```

It was specified by the source that certain data was faulty/irrelevant,
so I removed that data as well as negative/extremely high values for
**ride\_length\_seconds** and created a separate, final dataframe called
**bike\_rides\_v2**.

``` r
bike_rides_v2 <- bike_rides[!(bike_rides$start_station_name == "HQ QR" | bike_rides$ride_length_seconds<0 | bike_rides$ride_length_seconds>604800),]
```

Finally, I corrected the order of the days of the week so my
visualizations would be more intuitive.

``` r
bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
```

Now, onto the analysis!

## Analysis

Check the mean, median, max, min of **ride\_length\_seconds**.

``` r
summary(bike_rides_v2$ride_length_seconds)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     476     874    1557    1601  602760

Compare the above measures between members and casual users

``` r
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = mean)
```

    ##   bike_rides_v2$member_casual bike_rides_v2$ride_length_seconds
    ## 1                      casual                         2424.4657
    ## 2                      member                          953.5629

``` r
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = median)
```

    ##   bike_rides_v2$member_casual bike_rides_v2$ride_length_seconds
    ## 1                      casual                              1272
    ## 2                      member                               689

``` r
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = max)
```

    ##   bike_rides_v2$member_casual bike_rides_v2$ride_length_seconds
    ## 1                      casual                            602760
    ## 2                      member                            595314

``` r
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual, FUN = min)
```

    ##   bike_rides_v2$member_casual bike_rides_v2$ride_length_seconds
    ## 1                      casual                                 0
    ## 2                      member                                 0

Find the average ride time by each day for members vs. casuals.

``` r
aggregate(bike_rides_v2$ride_length_seconds ~ bike_rides_v2$member_casual + bike_rides_v2$day_of_week, FUN = mean)
```

    ##    bike_rides_v2$member_casual bike_rides_v2$day_of_week
    ## 1                       casual                    Sunday
    ## 2                       member                    Sunday
    ## 3                       casual                    Monday
    ## 4                       member                    Monday
    ## 5                       casual                   Tuesday
    ## 6                       member                   Tuesday
    ## 7                       casual                 Wednesday
    ## 8                       member                 Wednesday
    ## 9                       casual                  Thursday
    ## 10                      member                  Thursday
    ## 11                      casual                    Friday
    ## 12                      member                    Friday
    ## 13                      casual                  Saturday
    ## 14                      member                  Saturday
    ##    bike_rides_v2$ride_length_seconds
    ## 1                          2694.2041
    ## 2                          1057.9720
    ## 3                          2446.0918
    ## 4                           903.9466
    ## 5                          2228.6534
    ## 6                           902.4670
    ## 7                          2183.0945
    ## 8                           904.9901
    ## 9                          2233.8757
    ## 10                          902.5775
    ## 11                         2302.0529
    ## 12                          945.7863
    ## 13                         2573.4333
    ## 14                         1054.5039

Here, we analyze ridership data by type and weekday

``` r
bike_rides_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual,weekday) %>%  #groups by user type and weekday
  summarize(number_of_rides = n(), average_duration = mean(ride_length_seconds)) %>%  #calculations
  arrange(weekday)  
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the `.groups` argument.

    ## # A tibble: 14 x 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int>            <dbl>
    ##  1 casual        Sun              262203            2694.
    ##  2 member        Sun              265293            1058.
    ##  3 casual        Mon              151140            2446.
    ##  4 member        Mon              267333             904.
    ##  5 casual        Tue              145244            2229.
    ##  6 member        Tue              284370             902.
    ##  7 casual        Wed              158372            2183.
    ##  8 member        Wed              305094             905.
    ##  9 casual        Thu              166361            2234.
    ## 10 member        Thu              300451             903.
    ## 11 casual        Fri              208511            2302.
    ## 12 member        Fri              306397             946.
    ## 13 casual        Sat              334995            2573.
    ## 14 member        Sat              323118            1055.

Check how many trips there are each day with each type of bike for
members vs. casuals.

``` r
bike_rides_v2 %>% 
  count(member_casual,day_of_week, rideable_type)
```

    ##    member_casual day_of_week rideable_type      n
    ## 1         casual      Sunday  classic_bike  14027
    ## 2         casual      Sunday   docked_bike 210138
    ## 3         casual      Sunday electric_bike  38038
    ## 4         casual      Monday  classic_bike   8992
    ## 5         casual      Monday   docked_bike 114609
    ## 6         casual      Monday electric_bike  27539
    ## 7         casual     Tuesday  classic_bike   8370
    ## 8         casual     Tuesday   docked_bike 109221
    ## 9         casual     Tuesday electric_bike  27653
    ## 10        casual   Wednesday  classic_bike   7586
    ## 11        casual   Wednesday   docked_bike 119858
    ## 12        casual   Wednesday electric_bike  30928
    ## 13        casual    Thursday  classic_bike   6160
    ## 14        casual    Thursday   docked_bike 128172
    ## 15        casual    Thursday electric_bike  32029
    ## 16        casual      Friday  classic_bike   7507
    ## 17        casual      Friday   docked_bike 163656
    ## 18        casual      Friday electric_bike  37348
    ## 19        casual    Saturday  classic_bike  18158
    ## 20        casual    Saturday   docked_bike 267459
    ## 21        casual    Saturday electric_bike  49378
    ## 22        member      Sunday  classic_bike  30748
    ## 23        member      Sunday   docked_bike 190096
    ## 24        member      Sunday electric_bike  44449
    ## 25        member      Monday  classic_bike  37144
    ## 26        member      Monday   docked_bike 181577
    ## 27        member      Monday electric_bike  48612
    ## 28        member     Tuesday  classic_bike  37956
    ## 29        member     Tuesday   docked_bike 195053
    ## 30        member     Tuesday electric_bike  51361
    ## 31        member   Wednesday  classic_bike  38304
    ## 32        member   Wednesday   docked_bike 211349
    ## 33        member   Wednesday electric_bike  55441
    ## 34        member    Thursday  classic_bike  33151
    ## 35        member    Thursday   docked_bike 211435
    ## 36        member    Thursday electric_bike  55865
    ## 37        member      Friday  classic_bike  33911
    ## 38        member      Friday   docked_bike 215523
    ## 39        member      Friday electric_bike  56963
    ## 40        member    Saturday  classic_bike  37857
    ## 41        member    Saturday   docked_bike 229668
    ## 42        member    Saturday electric_bike  55593

Check how the times that casuals/members start trips are different.

``` r
bike_rides_v2 %>% 
  count(member_casual,start_hour)
```

    ##    member_casual start_hour      n
    ## 1         casual          0  22450
    ## 2         casual          1  13994
    ## 3         casual          2   7693
    ## 4         casual          3   4120
    ## 5         casual          4   3436
    ## 6         casual          5   5408
    ## 7         casual          6  12891
    ## 8         casual          7  23147
    ## 9         casual          8  31769
    ## 10        casual          9  40777
    ## 11        casual         10  58354
    ## 12        casual         11  80543
    ## 13        casual         12  98130
    ## 14        casual         13 106701
    ## 15        casual         14 113464
    ## 16        casual         15 119597
    ## 17        casual         16 126541
    ## 18        casual         17 139909
    ## 19        casual         18 126112
    ## 20        casual         19  96344
    ## 21        casual         20  67692
    ## 22        casual         21  48875
    ## 23        casual         22  43734
    ## 24        casual         23  35145
    ## 25        member          0  12116
    ## 26        member          1   6804
    ## 27        member          2   3663
    ## 28        member          3   2328
    ## 29        member          4   3586
    ## 30        member          5  17278
    ## 31        member          6  56395
    ## 32        member          7  94634
    ## 33        member          8 102791
    ## 34        member          9  87114
    ## 35        member         10  92584
    ## 36        member         11 115963
    ## 37        member         12 136278
    ## 38        member         13 135845
    ## 39        member         14 135977
    ## 40        member         15 150820
    ## 41        member         16 180127
    ## 42        member         17 215546
    ## 43        member         18 187086
    ## 44        member         19 130276
    ## 45        member         20  80057
    ## 46        member         21  47975
    ## 47        member         22  33777
    ## 48        member         23  23036

## Visualizations

## ![avg\_duration\_per\_day](https://github.com/aaronjoslinwangdu/bike-share-case-study/blob/master/Visualizations/avg_duration_per_day.png)

## ![number\_of\_rides\_by\_rider\_type](https://github.com/aaronjoslinwangdu/bike-share-case-study/blob/master/Visualizations/number_of_rides_by_rider_type.png)

## ![rides\_every\_hour](https://github.com/aaronjoslinwangdu/bike-share-case-study/blob/master/Visualizations/rides_every_hour.png)

## Conclusion
