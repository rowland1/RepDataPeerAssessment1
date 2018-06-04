getwd()

#Code for reading in the dataset and transforming the data for analysis

list.files()
unzip("./repdata_data_activity.zip")
act_data <- read.csv("./activity.csv", header = T)
names(act_data)
str(act_data)
head(act_data)

#convert date to from integer to date format

act_data$date <- as.Date(act_data$date , "%Y-%m-%d")

#total steps per day

act_data.day <- aggregate(act_data$steps, list(act_data$date), sum)
names(act_data.day)[1] <- "day"
names(act_data.day)[2] <- "steps"

#total steps per interval

act_data.interval <- aggregate(act_data$steps, list(act_data$interval), sum, na.rm=TRUE, na.action=NULL)
names(act_data.interval)[1] <- "interval"
names(act_data.interval)[2] <- "steps"

#mean steps per interval

act_data.mean.interval <- aggregate(act_data$steps, list(act_data$interval), mean, na.rm=TRUE, na.action=NULL)
names(act_data.mean.interval) [1] <- "interval"
names(act_data.mean.interval) [2] <- "mean.steps"

#Histogram of the total number of steps taken each day

hist(act_data.day$steps, 
     main = "Historgram of Total Steps Taken Each Day",
     xlab = "Total Steps Taken Each Day")

#Mean and median number of steps taken each day

mean(act_data.day$steps, na.rm = TRUE)

median(act_data.day$steps, na.rm = TRUE)

#Average number of steps taken

plot(act_data.mean.interval$interval, act_data.mean.interval$mean.steps, type = "n", 
     main = "Steps Taken per 5-Minute Interval", 
     xlab = "5-Minute Intervals", 
     ylab = "Average Number of Steps Taken")
lines(act_data.mean.interval$interval, act_data.mean.interval$mean.steps, type = "l")

#The 5-minute interval that, on average, contains the maximum number of steps

act_data.interval[which.max(act_data.mean.interval$mean.steps),1]

#Code to describe and show a strategy for imputing missing data

#Number of missing values in the dataset

sum(is.na(act_data$steps))

#Impute missing data and replace NA's with imputed values

act_data.imput <- merge(act_data, act_data.mean.interval, by = "interval", sort = FALSE)
act_data.imput <- act_data.imput[with(act_data.imput, order(date,interval)), ] 
act_data.imput$steps[is.na(act_data.imput$steps)] <- act_data.imput$mean.steps[is.na(act_data.imput$steps)]
act_data.imput$mean.steps <- NULL

head(act_data)
head(act_data.mean.interval)
head(act_data.imput)

act_data.imput$steps <- round(act_data.imput$steps, digits = 0)

#Create a new data set with NA's replaced and rounded

act_data.new <- act_data.imput[, c(2,3,1)]

#Histogram of the total number of steps taken each day after missing values are imputed

#total steps per day with NA's replaced with imputed values

act_data.day.new <- aggregate(act_data.new$steps, list(act_data.new$date), sum)
names(act_data.day.new)[1] <- "day"
names(act_data.day.new)[2] <- "steps"


hist(act_data.day.new$steps, 
     main = "Historgram of Total Steps Taken Each Day (NA's replaced)",
     xlab = "Total Steps Taken Each Day")


#Mean and median of total nubmer of steps taken per day with NA's replaced

mean(act_data.day.new$steps, na.rm = TRUE)

median(act_data.day.new$steps, na.rm = TRUE)

par(mfrow = c(1,2))

hist(act_data.day$steps, 
     main = "Historgram of Total Steps Taken Each Day with NA's",
     xlab = "Total Steps Taken Each Day")

hist(act_data.day.new$steps, 
     main = "Historgram of Total Steps Taken Each Day (NA's replaced)",
     xlab = "Total Steps Taken Each Day")


#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

act_data.new.v2 <- act_data.new
Sys.setlocale("LC_TIME", "English")
act_data.new.v2$weekdays <- factor(format(act_data.new.v2$date, '%A'))
levels(act_data.new.v2$weekdays)
levels(act_data.new.v2$weekdays) <- list("weekday" = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekend" = c("Saturday", "Sunday"))

head(act_data.new.v2)

act_data.new.v2.mean.interval <- aggregate(act_data.new.v2$steps, by=list(act_data.new.v2$weekdays, act_data.new.v2$interval), mean, na.rm=TRUE, na.action=NULL)
names(act_data.new.v2.mean.interval)[1] <- "Weekday"
names(act_data.new.v2.mean.interval)[2] <- "Interval"
names(act_data.new.v2.mean.interval)[3] <- "Mean_Steps"

library(lattice)
xyplot(act_data.new.v2.mean.interval$Mean_Steps ~ act_data.new.v2.mean.interval$Interval | act_data.new.v2.mean.interval$Weekday,
          layout=c(1,2), 
          type= "l", 
          xlab = "Interval", 
          ylab = "Number of Days")




