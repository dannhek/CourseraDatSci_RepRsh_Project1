#Assume we've got the correct working directory.
activity <- read.csv("activity.csv")
summary(activity) #What are we looking at here?
typeof(activity$interval) 
activity$interval <- as.factor(as.character(activity$interval)) #this should really be a factor instead of an integer

## What is mean total number of steps taken per day?
#First, let's get the data aggregated by day
library(ggplot2); library(plyr)
activity.byDay <- ddply(activity,.(date),summarize,steps=sum(steps))
head(activity.byDay)
