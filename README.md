# sds315_hw2

```r
knitr::opts_chunk$set(echo = TRUE)
```

```r
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(lubridate)
```
## Problem 1 
### Part A 

```r
#load data set
profs <- read_csv("profs.csv")

#create plot to display overall data distribution of eval scores
profs|>
ggplot() +
  geom_histogram(aes(
    x=eval),
     bins=30,color='black', fill='paleturquoise3')+
    labs(title="Distribution of Course Evaluation Scores",
         x="Scores",
         y="Frequency") +
  theme_minimal()
```
### Part B 
```r #create plot to show distribution eval score based on native english or not
profs|>
  ggplot() +
  geom_boxplot(aes(x=native, y=eval), fill='paleturquoise3')+
    labs(
      title="Course Evaluation Scores: Non-Native vs. Native English Speakers",
      x="Native English Speaker or No",
      y="Score")+
  theme_minimal()
```
### Part C
``` r
#create faceted histogram with two rows to compare distribution of course eval scores for male and female instructors 
profs|>
  ggplot()+
  geom_histogram(aes(x=eval), bins=30, fill='paleturquoise3')+
  facet_wrap(~gender, nrow=2)+
  labs(
    title="Course Evaluation Scores: Female vs. Male",
    x= "Score",
    y= "Frequency")+
  theme_minimal()
```
### Part D
``` r
profs|>
ggplot()+
  geom_point(aes(x=beauty, y=eval), color='paleturquoise4')+
  labs( 
    title="Professor's Phsyical Attractiveness vs Course Evaluation Score",
    y="Score",
    x="Avg. Rating of Physical Attractiveness")+
  theme_minimal()
```


## Problem 2 
### Plot A
``` r
#load data set 
bikeshare <- read_csv("bikeshare.csv")

#create subset
avg_hr <- bikeshare |>
  group_by(hr) |>
  summarize(avgRentals= mean(total))

#create plot to show hourly bike rentals across hours of the day
avg_hr|>
  ggplot()+
  geom_line(aes(y=avgRentals, x=hr), color='paleturquoise3')+
  labs(
    title="Average Hourly Bike Rentals Across All Hours of the Day",
    y= "Avg # of Bike Rentals",
    x="Hour"
  )+
  theme_minimal()
```
### Plot B
``` r
#create new subset that has working day vs non working day within each hour of the day
avg_working_day <- bikeshare |>
  group_by(hr, workingday) |>
  summarize(avgRentals = mean(total))

#create plot   
ggplot(data=avg_working_day)+
  geom_line(aes(x=hr, y=avgRentals), color='paleturquoise')+
         facet_wrap(~workingday, labeller = as_labeller(c("1"= "Working Day", "0"= "Not a Working Day")))+
  labs(
    title= "Avg Hourly Bike Rentals by Working Day or Not",
    x="Hour",
    y="Avg # of Bike Rentals"
  )+
  theme_minimal()
```
### Plot C
``` r
#create subset 
ridership_9 <- bikeshare |>
  filter(hr==8)|>
  group_by(weathersit, workingday) |>
  summarize(avgRentals = mean(total))
  
#create plot
ggplot(data=ridership_9)+
  geom_bar(aes(x=factor(weathersit),y= avgRentals, fill=factor(weathersit)),
           stat="identity",
           color='paleturquoise')+
  facet_wrap(~workingday, labeller= as_labeller(c("1"= "Working Day", "0"= "Not a Working Day")))+
  labs(
    title = "Average Ridership During 9 AM by Weather Situation",
    x = "Weather Situation Code",
    y = "Average Rentals",
    fill = "Weather Code"
  ) +
  theme_minimal()
```


## Problem 3 
### 1. 
``` r
#load data set 
capmetro <- read_csv("capmetro_UT.csv")

#create subset 
avg_boarding <- capmetro |>
  group_by(day_of_week, hour_of_day, month) |>
  summarize(avg_hourly_boarding= mean(boarding), .groups = "drop")|>
  mutate(day_of_week = factor(day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu", "Fri", "Sat")))|>
  mutate(month=factor(month,levels=c("Sep","Oct","Nov")))

#create plot 
ggplot(data=avg_boarding)+
  geom_line(aes(y=avg_hourly_boarding, x=hour_of_day, color=factor(month)))+
  facet_wrap(~day_of_week)+
  labs(
    title="Average Boarding by Hour of the Day, Day of Week, & Month",
    y="Average # of Boardings",
    x="Hour",
    color="Month"
  )+
  theme_minimal()
```
### 2. 
``` r
#create subset 
boarding_temp <- capmetro |>
  mutate(weekend_or_weekday= "Weekday")|>
  mutate(weekend_or_weekday=replace(weekend_or_weekday, day_of_week=="Sat"|day_of_week=="Sun", "Weekend"))|>
  group_by(day_of_week,temperature, hour_of_day)

#create plot 
ggplot(data=boarding_temp)+
  geom_point(aes(y=boarding, x=temperature, color=weekend_or_weekday))+
  facet_wrap(~hour_of_day)+
  labs(
    title="Boardings vs. Temperarture by Hour of the Day",
    y="Boardings",
    x="Temperature",
    color="Weekend or Weekday")+
  theme_minimal()
```


## Problem 4 
### Part A
``` r
#load data set
billboard <- read_csv("billboard.csv")

#create table 
top_10 <- billboard |>
  group_by(performer,song)|>
  summarize(count=n(),.groups="drop") |>
  arrange(desc(count)) |>
  slice_head(n=10) |>
  select(performer, song, count)

kable(top_10, col.names=c("performer", "song", "count"), caption="Top 10 Most Popular Songs Since 1958")
```
### Part B
``` r
#create subset 
filtered_years <- billboard |>
  filter(year!=1958 & year!=2021)|>
  group_by(year)|>
  summarize(unique=n_distinct(song),.groups="drop")

#create plot 
ggplot(data=filtered_years) +
  geom_line(aes(x=year,y=unique))+
  labs(
    title="Measure of Musical Diversity over the Years",
    x="Year",
    y="# of Unique Songs"
  )+
  theme_minimal()
```

### Part C 
``` r
#create subset and wrangle
ten_week<- billboard |>
  group_by(performer,song )|>
  summarize(weeks_on_chart= n(),.groups="drop")|>
  filter(weeks_on_chart>=10)|>
  count(performer, name="ten_week_hits")|>
  filter(ten_week_hits>=30)|>
  arrange(desc(ten_week_hits))

#create plot 
ggplot(data=ten_week)+
  geom_bar(aes(x=performer, y=ten_week_hits), stat="identity")+
  labs(
    title = "Performers with 30+ Songs on the Billboard for 10+ Weeks",
    x = "Performer",
    y = "Number of Songs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```


