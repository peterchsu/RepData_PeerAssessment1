readData <- function(){
  if(!file.exists("activity.csv")){
    unzip("activity.zip")
  }
  activity <- read.csv("activity.csv")
  activity$date <- as.Date(activity$date)
  
  activity
}

dailysteps <- aggregate(steps ~ date, data=activity, sum)

with(dailysteps, hist(steps, main="Total number of steps taken each day",
                      xlab = "Steps"))

qplot(steps, data=dailysteps)