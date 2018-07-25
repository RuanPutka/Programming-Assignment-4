library(dplyr)
library(reshape)

setwd("~/R Projects/R Programming/Programming Assignment 4")

## Reads a 561-feature vector with time and frequency domain variables, getting
## the features names to use as columns in the training and test dataset.
features.vector <- read.table("UCI HAR Dataset//features.txt")
features.names <- as.character(features.vector[,2])

## Merges the training and the test sets to create one dataset.
df <- rbind(read.table("UCI HAR Dataset//train//X_train.txt"),
           read.table("UCI HAR Dataset//test//X_test.txt"))

## Extracts only the measurements of the mean and standard deviation
## for each observation.
names(df) <- features.names
new.names <- features.names[grepl("[Ss][Tt][Dd]|[Mm][Ee][Aa][Nn]",
                                     features.names)]
df <- df[,new.names]

## Setting descriptive activity names to name the activities in the dataset.
activity.levels <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", 
                            "SITTING", "STANDING", "LAYING")
labels <- rbind(read.table("UCI HAR Dataset//train//y_train.txt"),
                read.table("UCI HAR Dataset//test//y_test.txt"))
labels <- sapply(labels, function(e) activity.levels[e])

## Merges df, labels and subjects.
subjects <- rbind(read.table("UCI HAR Dataset//train//subject_train.txt"),
                  read.table("UCI HAR Dataset//test//subject_test.txt"))
df <- cbind(df, labels, subjects)

## Appropriately labels the data set with descriptive variable names
new.names = gsub('-[Mm]ean*', 'Mean', new.names)
new.names = gsub('-[Ss]td*', 'Std', new.names)
new.names <- gsub('[-()]', '', new.names)

names(df) <- c(new.names, "ActivityLevel", "Subject")

## Turn activities and subjects into factors.
df$ActivityLevel <- as.factor(df$ActivityLevel)
df$Subject <- as.factor(df$Subject)

## Creates a second, independent tidy data set with the average of each 
## variable for each activity and each subject.
df.melted <- melt(df, id = c("ActivityLevel", "Subject"))
df.tiny <- cast(df.melted, Subject + ActivityLevel ~ variable, mean)
write.table(df.tiny, "tiny_dataset.txt", row.name=FALSE)