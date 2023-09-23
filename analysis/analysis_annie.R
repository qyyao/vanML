library(tidyverse)
library(dplyr)

data <- readRDS('merged.RDS')

#remove two outliers
data <- filter(data, CallLength < 600)

#by gender
ggplot(data = data, aes(x = CallerDemographicsGender, y = CallLength)) + geom_boxplot()
#outliers in female may skew their avg call times 

#by city
ggplot(data = data, aes(x = CityName, y = CallLength)) + geom_boxplot(outlier.colour = "blue")
#consider a physical map for this

#by age
ggplot(data = data, aes(x = CallerDemographicsAgeGroup, y = CallLength)) + geom_boxplot()
#turn age into numerical data?
#incredibly skewed - consider log transform

#by date
avg_call_by_date <- data %>%
  group_by(DateStart) %>%
  summarize(meanCallTime = mean(CallLength))
ggplot(data = avg_call_by_date, aes(x = DateStart, y = meanCallTime)) + geom_point()

#by reason
ggplot(data = data, aes(x = Mainreason, y = CallLength)) + geom_boxplot()



###LOGGING 
data$log_length = log(data$CallLength)

#by gender
ggplot(data = data, aes(x = CallerDemographicsGender, y = log_length)) + geom_boxplot()
#outliers in female may skew their avg call times 

#by city
ggplot(data = data, aes(x = CityName, y = log_length)) + geom_boxplot(outlier.colour = "blue")
#consider a physical map for this

#by age
ggplot(data = data, aes(x = CallerDemographicsAgeGroup, y = log_length)) + geom_boxplot()
#turn age into numerical data?
#incredibly skewed - consider log transform

#by date
avg_call_by_date <- data %>%
  group_by(DateStart) %>%
  summarize(meanCallTime = mean(log_length))
ggplot(data = avg_call_by_date, aes(x = DateStart, y = meanCallTime)) + geom_point()

#by reason
avg_call_by_date <- data %>%
  group_by(Mainreason) %>%
  summarize(meanCallTime = mean(log_length))
ggplot(data = avg_call_by_date, aes(x = Mainreason, y = meanCallTime)) + geom_boxplot()

#by time of call
data$TimeStart <- sub(".*? ", "", data$TimeStart)
avg_call_by_date <- data %>%
  group_by(TimeStart) %>%
  summarize(meanCallTime = mean(log_length))
ggplot(data = avg_call_by_date, aes(x = TimeStart, y = meanCallTime)) + geom_point()

#assessing: call hour
data$callhour <- as.numeric(sub(":.+", "", data$TimeStart))

calls_per_hour <- data %>%
  group_by(DateStart, callhour) %>%
  summarize(count = n(), avg_calllength = mean(CallLength)) 

ggplot(data = calls_per_hour, aes(x = count, y = avg_calllength)) + geom_point()































