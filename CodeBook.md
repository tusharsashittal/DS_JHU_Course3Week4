## This section discusses the transformations that the two data sets went through before they reached their final state

setwd("C:/Users/SashitTS/R_REPOSITORIES/JHUCourse3Week4")

# Import the training data into R
X_train <- read.fwf("./UCI HAR Dataset/train/X_train.txt", widths=c(rep(16, 561)), header = FALSE)
Y_train <- read.csv("./UCI HAR Dataset/train/Y_train.txt", sep = " ", header = FALSE)
subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", sep = " ", header = FALSE)

# Import feature i.e. variable names into R
columns <- read.csv("./UCI HAR Dataset/features.txt", sep = "", header = FALSE)
columnNames <- columns[, 2]
names(X_train) <- columnNames

# Select the variables that have mean() or std() in them to complete step 2 of the instructions for the training data
X_train_MNSD <- X_train[, grep("(mean\\(\\))|std\\(\\)", columnNames, ignore.case = TRUE, value = TRUE)]
X_train_MNSD_names <- names(X_train_MNSD)

# Add two variable names for the activity and subject  that will be merged into the training data
X_train_MNSD_names <- c(names(X_train_MNSD), c("ActivityID", "SubjectID"))

# Combine the training data with activity and subject data and assign the variable names to that data set
X_train_MNSD <- cbind(X_train_MNSD, Y_train, subject_train)
names(X_train_MNSD) <- X_train_MNSD_names

# Import the test data into R
X_test <- read.fwf("./UCI HAR Dataset/test/X_test.txt", widths=c(rep(16, 561)), header = FALSE)
Y_test <- read.csv("./UCI HAR Dataset/test/Y_test.txt", sep = " ", header = FALSE)
subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep = " ", header = FALSE)
names(X_test) <- columnNames

# Select the variables that have mean() or std() in them to complete step 2 of the instructions for the test data
X_test_MNSD <- X_test[, grep("(mean\\(\\))|std\\(\\)", columnNames, ignore.case = TRUE, value = TRUE)]
X_test_MNSD_names <- names(X_test_MNSD)

# Add two variable names for the activity and subject  that will be merged into the test data
X_test_MNSD_names <- c(names(X_test_MNSD), c("ActivityID", "SubjectID"))

# Combine the test data with activity and subject data and assign the variable names to that data set
X_test_MNSD <- cbind(X_test_MNSD, Y_test, subject_test)
names(X_test_MNSD) <- X_test_MNSD_names

# Combine the test and test data sets to complete step 1 of  the instructions
X_combined <- rbind(X_train_MNSD, X_test_MNSD)

# Read the activity labels into R
activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE)
names(activity_labels) <- c("ActivityID", "Activity_Description")

# Inner join the data set with the activity data to get the activity descriptions to complete step 3 of the instructions
X_combined_activity <- merge(X_combined, activity_labels, by.x = "ActivityID", by.y = "ActivityID", all = FALSE, all.x = FALSE, all.y = FALSE)

# Label the data set with descriptive variable names to complete step 4 of the instructions
X_combined_activity_names <- names(X_combined_activity)
X_combined_activity_names <- gsub("-std\\(\\)", "StandardDeviation", X_combined_activity_names, ignore.case = TRUE)
X_combined_activity_names <- gsub("-mean\\(\\)", "Mean", X_combined_activity_names, ignore.case = TRUE)
X_combined_activity_names <- gsub("-", "", X_combined_activity_names)
names(X_combined_activity) <- X_combined_activity_names 

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject
# This completes step 5 of the instructions
mColNames <- paste0("mean(", X_combined_activity_names, ")")
mColNames <- paste(mColNames[2:(length(mColNames)-3)], collapse = ", ")
X_combined_activity1 <- tbl_df(X_combined_activity) %>%
                        group_by(Activity_Description, SubjectID) %>% 
                        summarize(mean(tBodyAccMeanX), mean(tBodyAccMeanY), mean(tBodyAccMeanZ), mean(tBodyAccStandardDeviationX), mean(tBodyAccStandardDeviationY), mean(tBodyAccStandardDeviationZ), mean(tGravityAccMeanX), mean(tGravityAccMeanY), mean(tGravityAccMeanZ), mean(tGravityAccStandardDeviationX), mean(tGravityAccStandardDeviationY), mean(tGravityAccStandardDeviationZ), mean(tBodyAccJerkMeanX), mean(tBodyAccJerkMeanY), mean(tBodyAccJerkMeanZ), mean(tBodyAccJerkStandardDeviationX), mean(tBodyAccJerkStandardDeviationY), mean(tBodyAccJerkStandardDeviationZ), mean(tBodyGyroMeanX), mean(tBodyGyroMeanY), mean(tBodyGyroMeanZ), mean(tBodyGyroStandardDeviationX), mean(tBodyGyroStandardDeviationY), mean(tBodyGyroStandardDeviationZ), mean(tBodyGyroJerkMeanX), mean(tBodyGyroJerkMeanY), mean(tBodyGyroJerkMeanZ), mean(tBodyGyroJerkStandardDeviationX), mean(tBodyGyroJerkStandardDeviationY), mean(tBodyGyroJerkStandardDeviationZ), mean(tBodyAccMagMean), mean(tBodyAccMagStandardDeviation), mean(tGravityAccMagMean), mean(tGravityAccMagStandardDeviation), mean(tBodyAccJerkMagMean), mean(tBodyAccJerkMagStandardDeviation), mean(tBodyGyroMagMean), mean(tBodyGyroMagStandardDeviation), mean(tBodyGyroJerkMagMean), mean(tBodyGyroJerkMagStandardDeviation), mean(fBodyAccMeanX), mean(fBodyAccMeanY), mean(fBodyAccMeanZ), mean(fBodyAccStandardDeviationX), mean(fBodyAccStandardDeviationY), mean(fBodyAccStandardDeviationZ), mean(fBodyAccJerkMeanX), mean(fBodyAccJerkMeanY), mean(fBodyAccJerkMeanZ), mean(fBodyAccJerkStandardDeviationX), mean(fBodyAccJerkStandardDeviationY), mean(fBodyAccJerkStandardDeviationZ), mean(fBodyGyroMeanX), mean(fBodyGyroMeanY), mean(fBodyGyroMeanZ), mean(fBodyGyroStandardDeviationX), mean(fBodyGyroStandardDeviationY), mean(fBodyGyroStandardDeviationZ), mean(fBodyAccMagMean), mean(fBodyAccMagStandardDeviation), mean(fBodyBodyAccJerkMagMean), mean(fBodyBodyAccJerkMagStandardDeviation), mean(fBodyBodyGyroMagMean), mean(fBodyBodyGyroMagStandardDeviation), mean(fBodyBodyGyroJerkMagMean))
                        # summarize(mColNames[[1]])

# Output the two data sets to csv files
write.csv(X_combined_activity, file = "./X_combined_activity.csv")
write.csv(X_combined_activity1, file = "./X_combined_activity1.csv")

