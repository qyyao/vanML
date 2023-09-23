library(tidyverse)
library(dplyr)

# file to process data for the models
data <- readRDS('merged_cases.RDS')

set.seed(2)

#remove two outliers
data <- filter(data, CallLength < 600)

#log of call length
data$log_CallLength = log(1 + data$CallLength)

#processing time start
data$TimeStart <- sub(".*? ", "", data$TimeStart)

#call get call hour
data$CallHour <- as.numeric(sub(":.+", "", data$TimeStart))

#make time start numerical
data$TimeStart <- as.numeric(gsub("[: -]", "" , data$TimeStart, perl=TRUE))

#make age numerical
data <- data %>% mutate(age_numeric =
                     case_when(CallerDemographicsAgeGroup == '0-12 Child' ~ 1, 
                               CallerDemographicsAgeGroup == '13-18 Youth' ~ 2,
                               CallerDemographicsAgeGroup == '19-54 Adult' ~ 3, 
                               CallerDemographicsAgeGroup == '55-64 Adult' ~ 4, 
                               CallerDemographicsAgeGroup == '55-65 Older Adult' ~ 5, 
                               CallerDemographicsAgeGroup == '65+ Senior' ~ 6, 
                               .default = NULL)
)

#group cities by region 

#group by main reason 
others <- c("Ado", "Video Gaming", "Not Obtained", "Human Rights", "Other", "Arts Culture & Recreation", "Consumer & Commercial", "Complaints")

mental_health <- c("Gambling", "Social Isolation", "Suicide", "Grief/Loss", "Conflict with Others", "Mental Health", "Substance Use")

help <- c("Ukraine - displaced individual needing help", "Disaster", "Accident", "Missing Person", "Education/Training", "Employment", "Citizenship Immigration & Settlement", "Transportations", "Transportation", "Government Services", "Basic Needs", "Legal and Public Safety", "Income & Financial Assistance", "Health", "Housing and Homelessness")

crime <- c("Human Trafficking", "Offender Information", "Sex Trade", "Harrassment", "Bullying", "Criminal Harrassment", "Assault", "Other Crimes",  "Abuse") 

assistance <- c("Syrian Refugee Donations", "Ukraine - volunteering/donating to Ukrainians", "Advocacy", "Volunteer/Donations", "BC Hydro CCF")

data <- data %>% mutate(Broadreason =
                          case_when(Mainreason %in% others ~ "others", 
                                    Mainreason %in% mental_health ~ "mental_health", 
                                    Mainreason %in% help ~ "help", 
                                    Mainreason %in% crime ~ "crime", 
                                    Mainreason %in% assistance ~ "assistance", 
                                    .default = NULL)
)

data <- data %>% mutate(gender =
                          case_when(CallerDemographicsGender == "Male" ~ "Male", 
                                    CallerDemographicsGender == "Female" ~ "Female", 
                                    CallerDemographicsGender == "Trans Female" ~ "Transgender", 
                                    CallerDemographicsGender == "Trans Male" ~ "Transgender", 
                                    CallerDemographicsGender == "Transgender" ~ "Transgender", 
                                    .default = "other")
)


data <- dplyr::select(data, c('DateStart','TimeStart', "CallLength", "CityName", "CallType", "DateYear", "DateMonth", "New cases", "CallHour", "age_numeric", "Broadreason", "gender"))


#use 80% of dataset as training set and 20% as test set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train  <- data[sample, ]
test   <- data[!sample, ]

saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")
