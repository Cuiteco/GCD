# Project instruction 1: Merges the training and the test sets to create one data set
# Setting up the data directory as default
# setwd("E:/R/getdata/UCI") This is my computer's default directory and will be different on your's.

# Reading train data into a vestor in the memory of the system...
trainData <- read.table("./data/train/X_train.txt")

# Looking into dimesion of the dataset
dim(trainData)

# First five rows of the dataset...
head(trainData)

# Reading and setting up the rest of the data

trainLabel <- read.table("./data/train/y_train.txt")

table(trainLabel)

trainSubject <- read.table("./data/train/subject_train.txt")

testData <- read.table("./data/test/X_test.txt")
dim(testData)

testLabel <- read.table("./data/test/y_test.txt") 
table(testLabel) 

testSubject <- read.table("./data/test/subject_test.txt")

joinData <- rbind(trainData, testData)
dim(joinData)

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel)
head(joinLabel)

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)


#########################################################

# Project instruction 2: Extracts only the measurements on the mean and standard deviation for each measurement
features <- read.table("./data/features.txt")
dim(features)

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)

joinData <- joinData[, meanStdIndices]
dim(joinData)

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])

names(joinData) <- gsub("mean", "Mean", names(joinData))

names(joinData) <- gsub("std", "Std", names(joinData))

names(joinData) <- gsub("-", "", names(joinData))



####################################################################

# Project instruction 3: Uses descriptive activity names to name the activities in the data set

activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))

substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))

activityLabel <- activity[joinLabel[, 1], 2]

joinLabel[, 1] <- activityLabel

names(joinLabel) <- "activity"



####################################################################

# Project instruction 4: Appropriately labels the data set with descriptive activity names

names(joinSubject) <- "subject"

cleanedData <- cbind(joinSubject, joinLabel, joinData)

dim(cleanedData)

write.table(cleanedData, "merged_data.txt")




####################################################################

# Project instruction 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject

subjectLen <- length(table(joinSubject))

activityLen <- dim(activity)[1]

columnLen <- dim(cleanedData)[2]

result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 

result <- as.data.frame(result)

colnames(result) <- colnames(cleanedData)

row <- 1

for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    cool1 <- i == cleanedData$subject
    cool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[cool1&cool2, 3:columnLen])
    row <- row + 1
  }
}

head(result)

write.table(result, "data_with_means.txt", row.name=FALSE)
