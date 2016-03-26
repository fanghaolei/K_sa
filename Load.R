library(ggplot2)
library(lubridate)

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
sample <- read.csv("sample_submission.csv")

# exclude AnimalID and OutcomeSubtype from train
train <- train[, -c(1, 5)]


# exclude ID from test
test <- test[, -1]