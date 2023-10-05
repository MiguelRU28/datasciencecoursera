# datasciencecoursera

# After reading the prior data from a zip format.

# Step 1: Merges the training and the test sets to create one data set.

# Part 1 - Merge the training and the test sets to create one data set

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Naming the columns

colnames(features) <- t(featureNames[2])

# Merge the data

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# Part 2 - Extracts only the measurements on the mean and standard deviation for each 
# measurement

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Determine the dimension 

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

# We create extractedData with the selected columns in requiredColumns. 
# And again, we look at the dimension of requiredColumns.

extractedData <- completeData[,requiredColumns]
dim(extractedData)

# Part 3 - Uses descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)

for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

# We need to factor the activity variable, once the activity names are updated.

extractedData$Activity <- as.factor(extractedData$Activity)

# Part 4 - Appropriately labels the data set with descriptive variable names

names(extractedData)

# By examining extractedData, we can say that the following acronyms can be replaced:

# Acc can be replaced with Accelerometer
# 
# Gyro can be replaced with Gyroscope
# 
# BodyBody can be replaced with Body
# 
# Mag can be replaced with Magnitude
# 
# Character f can be replaced with Frequency
# 
# Character t can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

# Part 5 - From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# We create tidyData as a data set with average for each activity and subject.
# Then, we order the enties in tidyData and write it into data file Tidy.txt that 
# contains the processed data.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

