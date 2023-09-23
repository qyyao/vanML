# file to process data for the models
data <- readRDS('merged.RDS')

set.seed(1)

#remove two outliers
data <- filter(data, CallLength < 600)

#log of call length
data$log_CallLength = log(1 + data$CallLength)

#processing time start
data$TimeStart <- sub(".*? ", "", data$TimeStart)

#call length
data$CallLength <- as.numeric(sub(":.+", "", data$TimeStart))

#use 80% of dataset as training set and 20% as test set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train  <- data[sample, ]
test   <- data[!sample, ]

saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")
