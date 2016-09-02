# |*****************************************************************************
# | Dwayne Macadangdang 9/1/2016
# | Coursera: Getting and Cleaning Data
# | Week 4 Programming Assignment

# | You should create one R script called run_analysis.R that does the following.
# | 1. Merges the training and the test sets to create one data set.
# | 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# | 3. Uses descriptive activity names to name the activities in the data set
# | 4. Appropriately labels the data set with descriptive variable names.
# | 5. From the data set in step 4, creates a second, independent tidy data set with 
# |    the average of each variable for each activity and each subject.
# |*****************************************************************************


run_analysis <- function()
{
  print("Commencing analysis.")
  
  # set working directory, load library dependencies (assumes installation)
  setwd("/Users/mistermaxx/Documents/work/personal/Coursera/Getting_Cleaning_Data/Week_4/UCI_HAR_Dataset")
  library(data.table)
  library(dplyr)
    
  # read feature and acivity data from files
  feature.column.data <- read.table("features.txt")
  activity.column.data <- read.table("activity_labels.txt", header = FALSE)
  
  # read training data from files
  subject.training.data <- read.table("train/subject_train.txt", header = FALSE)
  activity.training.data <- read.table("train/y_train.txt", header = FALSE)
  feature.training.data <- read.table("train/X_train.txt", header = FALSE)
  
  # read test data from files
  subject.test.data <- read.table("test/subject_test.txt", header = FALSE)
  activity.test.data <- read.table("test/y_test.txt", header = FALSE)
  feature.test.data <- read.table("test/X_test.txt", header = FALSE)
  
  # combine test and training dta
  subject.data <- rbind(subject.training.data, subject.test.data)
  activity.data <- rbind(activity.training.data, activity.test.data)
  feature.data <- rbind(feature.training.data, feature.test.data)
  
  #preliminary columns
  colnames(feature.data) <- t(feature.column.data[2])
  colnames(activity.data) <- "Activity"
  colnames(subject.data) <- "Subject"
  
  # combine all data
  complete.data <- cbind(subject.data, activity.data, feature.data)
  
  # retain only the columns that contain subject, activity, mean and standard deviation metrics
  columns.with.mean.std <- grep(".*Mean.*|.*Std.*", names(complete.data), ignore.case=TRUE)
  required.columns <- c(1, 2, columns.with.mean.std)
  extracted.data <- complete.data[,required.columns]
  
  extracted.data <- data.table(extracted.data)
  
  # convert numeric activity values to descriptions
  extracted.data$Activity <- as.character(extracted.data$Activity)
  for (i in 1:6)
    {
      extracted.data$Activity[extracted.data$Activity == i] <- as.character(activity.column.data[i,2])
    }
  
  # clean up column names for readability
  names(extracted.data)<-gsub("Acc", "Accelerometer", names(extracted.data))
  names(extracted.data)<-gsub("angle", "Angle", names(extracted.data))
  names(extracted.data)<-gsub("BodyBody", "Body", names(extracted.data))
  names(extracted.data)<-gsub("-freq()", "Frequency", names(extracted.data), ignore.case = TRUE)
  names(extracted.data)<-gsub("^f", "Frequency", names(extracted.data))
  names(extracted.data)<-gsub("Gyro", "Gyroscope", names(extracted.data))
  names(extracted.data)<-gsub("gravity", "Gravity", names(extracted.data))
  names(extracted.data)<-gsub("Mag", "Magnitude", names(extracted.data))
  names(extracted.data)<-gsub("-mean()", "Mean", names(extracted.data), ignore.case = TRUE)
  names(extracted.data)<-gsub("-std()", "Standard", names(extracted.data), ignore.case = TRUE)
  names(extracted.data)<-gsub("^t", "Time", names(extracted.data))
  names(extracted.data)<-gsub("tBody", "TimeBody", names(extracted.data))
  
  # arrange, group and summarize data table using dplyr verbs
  extracted.data <- arrange(extracted.data, Subject, Activity)
  final.data <- group_by(extracted.data, Subject, Activity)  
  final.data.mean <- summarize_each(final.data, funs(mean))
  
  # write data to .csv for review in Excel
  write.csv(final.data.mean, file = "Tidy.txt", row.names = FALSE)
  
  print("Analysis complete.")
  
}
