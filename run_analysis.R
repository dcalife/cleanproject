run_analysis <- function() {
## load all the data files needed fromdirectory
features <- read.csv("features.txt", header = FALSE, sep = " ")
activitylabels <- read.csv("activity_labels.txt", header = FALSE, sep = "")

subject.test <- read.csv("test/subject_test.txt", header = FALSE, sep = "")
x.test <- read.csv("test/X_test.txt", header = FALSE, sep = "")
y.test <- read.csv("test/y_test.txt", header = FALSE, sep = "")

subject.train <- read.csv("train/subject_train.txt", header = FALSE, sep = "")
x.train <- read.csv("train/X_train.txt", header = FALSE, sep = "")
y.train <- read.csv("train/y_train.txt", header = FALSE, sep = "")

## merge train and test data sets
## first merge colunmns as subject, activity, features
DF.test <- cbind(subject.test, y.test)
DF.test <- cbind(DF.test, x.test)
DF.train <- cbind(subject.train, y.train)
DF.train <- cbind(DF.train, x.train)
## then, merge both sets
data.all <- rbind(DF.test, DF.train)

## label the variables
feature.label <- as.array(as.character(features[,2]))
colnames(data.all) <- c("subject", "activity", feature.label)

## find only means and stds
meanPosLabel <- grep("mean()", feature.label)
stdPosLabel <- grep("std()", feature.label)
pos.labels <- c(meanPosLabel, stdPosLabel)
data.filter <- data.all[, c("subject", "activity", feature.label[sort(pos.labels)])]

## use descriptive activities names
data.filter$activity <- as.character(activitylabels[data.filter$activity,2])

## use dplyr
## install.packages("dplyr")
library(dplyr)

## columns to group by
cols <- names(data.filter)[1:2]
## convert to symbol
dots <- lapply(cols, as.symbol)
## create the new data frame
result <- data.filter %>% group_by_(.dots=dots) %>% summarise_each(funs(mean))

result
}
