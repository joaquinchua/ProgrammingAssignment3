# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Load library and download files
library(data.table)
library(magrittr)
# packages <- c("data.table", "reshape2")
# sapply(packages, require, character.only=TRUE, quietly=TRUE)
WD <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(WD, "files.zip"))
unzip(zipfile = "files.zip")
rm(url)

# Read activity_labels, features
activitylabels <- fread(file.path(WD, "UCI HAR Dataset/activity_labels.txt"))
names(activitylabels) <- c("activityID", "activity")
features <- fread(file.path(WD, "UCI HAR Dataset/features.txt"))
names(features) <- c("featureID", "feature")

# Select features to keep and store names
featureskeepno <- grep("(mean|std)\\(\\)", features[, feature])
measurementname <- features[featureskeepno, feature]
measurementname <- gsub("\\(\\)", "", measurementname)


# Read train datasets
train <- fread(file.path(WD, "UCI HAR Dataset/train/X_train.txt"))
train <- train[, ..featureskeepno]
names(train) <- measurementname
trainactivity <- fread(file.path(WD, "UCI HAR Dataset/train/y_train.txt"))
names(trainactivity) <- "Activity"
trainsubj <- fread(file.path(WD, "UCI HAR Dataset/train/subject_train.txt"))
names(trainsubj) <- "SubjectID"
train <- cbind(trainsubj, trainactivity, train)
rm(trainsubj,trainactivity)

# Read test datasets
test <- fread(file.path(WD, "UCI HAR Dataset/test/X_test.txt"))
test <- test[, ..featureskeepno]
names(test) <- measurementname
testactivity <- fread(file.path(WD, "UCI HAR Dataset/test/Y_test.txt"))
names(testactivity) <- "Activity"
testsubj <- fread(file.path(WD, "UCI HAR Dataset/test/subject_test.txt"))
names(testsubj) <- "SubjectID"
test <- cbind(testsubj, testactivity, test)
rm(testsubj,testactivity)

# Merge train and test
merged <- rbind(train, test)


# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
merged[["Activity"]] <- factor(merged[, Activity], levels = activitylabels[,activityID], labels = activitylabels[,activity])


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
merged %<>% group_by(SubjectID,Activity) %>% summarise_all(mean)
fwrite(merged, "tidydataset.txt", quote = F)


