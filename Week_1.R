

install.packages("dplyr")
library(dplyr)
library(ggplot2)
setwd("D:\\Coursera\\Reproducible-Research\\repdata-data-activity")
### read file 
act <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
str(act)
act$date<- as.Date(act$date,'%Y-%m-%d')
act$steps<-as.numeric(act$steps)
head(act)
## What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day
##If you do not understand the difference between a histogram and a barplot,
##research the difference between them. Make a histogram of the total number of 
##steps taken each day
##Calculate and report the mean and median of the total number of 
##steps taken per day

nrow(act)

sum(is.na(act$steps))
sum(is.na(as.character(act$Date)) )
sum(is.na(act$interval))

act_nan<- act[which(!is.na(act$steps)),] ### removing null rows
str(act_nan)
act_nan
head(act_nan)

##perday_stps<-tapply(act_nan$steps,act_nan$date)

perday_stps<-aggregate(steps ~ date, act_nan , sum)

head(perday_stps)

png("perday_stps.png", width=520, height=480)

ggplot(perday_stps, aes(x= steps)) +
  geom_histogram(fill="lightblue",binwidth=800) + 
  ggtitle('Total number of steps taken per day') + xlab("Steps") +
  ylab("frequency")
dev.off()

mean(perday_stps$steps, na.rm = TRUE) ###10766.19
median(perday_stps$steps, na.rm = TRUE)### 10765

###What is the average daily activity pattern?

avg_intr_stpes<-aggregate(steps ~ interval, act_nan , mean)

png("avg_step.png", width=520, height=480)

 ggplot(avg_intr_stpes , aes(interval, steps, color = "red")) +
   geom_line() + xlab("year") + ylab("Average number of steps") +
   ggtitle(" Daily Activity Pattern")
 dev.off()
 
head(avg_intr_stpes)
max(avg_intr_stpes$steps)#### max steps 206.1698

### Imputing missing values
## total NA 
sum(is.na(act$steps))   ##2304

sum(is.na(act)) ##2304
head(act)
act_all <- act
act_all[which(is.na(act_all$steps)),1]<- mean(act_all$steps, na.rm = TRUE)
sum(is.na(act_all)) ## no of null 0 

max(act_all$interval) ##2355

##What is the impact of imputing missing data 
##on the estimates of the total daily number of steps?

png("Compare_null_no_null.png", width=640, height=480)

par(mfrow=c(1,2))


hist(perday_stps$steps,10,
     main = "Total number of steps taken per day", xlab = "Steps",ylim =c(0, 25)
     ) 
abline(v=median(perday_stps$steps),col = 4, lwd = 4)


perday_stps_nona <-aggregate(steps ~ date, act_all , sum)

 
hist(perday_stps_nona$steps,10,
     main = "Total number of steps taken per day
      After filling  NULL", xlab = "Steps",ylim =c(0, 25) 
) 
abline(v=median(perday_stps_nona$steps),col = 4, lwd = 4)

dev.off()

mean(perday_stps$steps) ## 10766.19
median(perday_stps$steps)## 10765

mean(perday_stps_nona$steps) ##10766.19
median(perday_stps_nona$steps) ## 10766.19

mean(perday_stps$steps)-mean(perday_stps_nona$steps) ###0 
median(perday_stps$steps)-median(perday_stps_nona$steps)## small difference

###Are there differences in activity patterns between weekdays and weekends?
act_all$date<- as.Date(act_all$date,'%Y-%m-%d')
act_all<- mutate(act_all, wk_typ= ifelse(weekdays(act_all$date)
        == "Saturday" | weekdays(act_all$date) =="Sunday", "weekend",
        "weekday"))
head(act_all)

act_all$wk_typ<- as.factor(act_all$wk_typ)

head(act_all)

act_weekty<- aggregate(steps ~ interval + wk_typ , act_all , mean)

png("weekly-comapre.png", width=640, height=640)
ggplot(act_weekty, aes(x=interval,y=steps ,color =wk_typ)) +
      geom_line() + ggtitle( "Daily Activity Pattern on Weekday & Weekend ") +
      facet_wrap(~wk_typ, ncol = 1, nrow=2)
dev.off() 

png("weekly-comapre_2.png", width=640, height=480)
ggplot(act_weekty, aes(x=interval,y=steps ,color =wk_typ)) +
  geom_line() + ggtitle( "Daily Activity Pattern on Weekday & Weekend ")  
dev.off() 