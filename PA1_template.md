# Reproducible Research: Peer Assessment 1 - Personal Activity Monitor
Arun Trivedi  
November 9, 2015  
 Background : 
 Personal activity monitoring device data collected at 5 minute interval between Oct-Nov 2012  
 Dataset : 
 Activity Monitoring Data [activity.csv : size 52k] 
---

## Loading and preprocessing the data

'''{r, echo = TRUE}   
library(dplyr)  
library(tidyr)  
library(ggplot2)    
library(Hmisc)  
setwd("c:/users/username/datasciencecoursera/rr")  
unzip("./RepData_PeerAssessment1/activity.zip")  
act_data <- read.csv("activity.csv", header = TRUE, sep = ",", dec = ".", na.strings = "NA", fill = TRUE)  
'''  

## What is mean total number of steps taken per day?  

'''{r, echo = TRUE}   
 actSum = tapply(act_data$steps, act_data$date, sum, na.rm = T)  
'''  

* Histogram of total number of steps per day *(Refer Figure : Total_Number_of_Steps_Per_Day.png)  

'''{r, echo = TRUE}   
 qplot(actSum, main = "Total number of steps per day", xlab = "Daily Total Steps", ylab = "Frequency (bindwidth = 500)", binwidth = 500)  
'''  

* Calculate mean and median total number of steps per day  

'''{r, echo = TRUE}   
actMean <- mean(actSum)  
actMedian <- median(actSum)    
'''  

- Mean r actMean
- Median  r actMedian 

## What is the average daily activity pattern?  

1. Time series plot (Refer figure : Time_Series_Plot.png)  

'''{r, echo = TRUE}   
 avgactPatt <- aggregate (x=list(meanSteps=act_data$steps), by=list(interval=act_data$interval), FUN = mean, na.rm=T)    
 g = ggplot(avgactPatt, aes(interval, meanSteps))    
 g + geom_line() +    
  labs (title = "Time Series Plot") +  
  xlab ("5 min Interval") +  
  ylab ("Average Steps Taken")  
'''  

2. Which 5 min. interval has max. number of steps?  

'''{r, echo = TRUE}   
 max <- which.max(avgactPatt$meanSteps)   
 maxInterval <- gsub ("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgactPatt[max, "interval"])  
'''  

## Imputing missing values  

### calculate & report missing values in the dataset  

'''{r, echo = TRUE}   
 NAact_data <- act_data %>% group_by(date) %>% filter(is.na(steps))    
'''  
 * total 2304 observations during 8 days  
 
'''{r, echo = TRUE}   
 ymd(unique(NAact_data$date))   
 [1] "2012-10-01 UTC" "2012-10-08 UTC" "2012-11-01 UTC" "2012-11-04 UTC" "2012-11-09 UTC"    
 [6] "2012-11-10 UTC" "2012-11-14 UTC" "2012-11-30 UTC"    
'''  

3. Create a new dataset  that is equal to original datset with missing data filled in with a strategy  

'''{r, echo = TRUE}   
 impute_act_data <- act_data  
 impute_act_data$steps <- impute (act_data$steps, fun=mean)  
'''  

4. Histogram of total number of steps taken each day (Refer Figure : total_steps_taken_per_day_imputed.png)  

'''{r, echo = TRUE}   
 stepsimpute_act_data <- tapply(impute_act_data$steps, impute_act_data$date, sum)  
 qplot(stepsimpute_act_data, main = "Total number of steps taken each day - Dataset Imputed", xlab="Steps/day ", ylab = "binwidth 500", binwidth=500)   
 stepsimpute_act_data <- mean(stepsimpute_act_data)  
 stepsimpute_act_data <- median(stepsimpute_act_data)  
''' 

## Are there differences in activity patterns between weekdays and weekends?    

1. create a new factor variable for two levels - weekdays and weekend  

'''{r, echo = TRUE}      
 impute_act_data$datetype <- ifelse(as.POSIXlt(impute_act_data$date)$wday %in% c(0,6), 'weekend', 'weekday'))   
'''  

2. Make a plot containing a time series plot (Refer Figure :
Imputed_Time_Series_plot.png)  

'''{r, echo = TRUE}   
 avg_impute_act_data <- aggregate(steps ~ interval + datetype, data = impute_act_data, mean)  
 ggplot(avg_impute_act_data, aes(interval, steps)) +  
  geom_line () +  
  facet_grid(datetype ~.) +  
  labs (title = "Imputed Time Series Plot") +  
  xlab ("5-minute interval") +  
  ylab ("Average number of steps")  
'''  
