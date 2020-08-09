library(data.table)
library(dplyr)

codebook <- function(...){
        cat(..., "\n",file=targetCodebookFilePath,append=TRUE, sep="")
}

fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'


#codebook
targetCodebookFilePath <- "./CodeBook.md"
file.remove(targetCodebookFilePath)
codebook("# Code Book")
codebook("generated ",as.character(Sys.time())," during sourcing of `run_analysis.R`")
codebook("")  
if(!exists("keyColumns")){
        keyColumns <<- c()
}


if(!exists("featureColumns")){
        featureColumns <<- c()
}


codebook("## Actions performed on data:")

codebook("* downloading zip file: ", fileurl)

if (!file.exists('./UCI HAR Dataset.zip')){
        download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
        unzip("UCI HAR Dataset.zip", exdir = getwd())
}

codebook("* merging all *_test.txt and *_train.txt files into one dataset: `Merged_Data`")

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#Step 1: Merges the training and the test sets to create one data set.

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)


#Step 2:Extracts only the measurements on the mean and standard deviation for each measurement.
codebook("*Extracts only the measurements on the mean and standard deviation for each measurement.")

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set.
codebook("*Uses descriptive activity names to name the activities in the data set.")

TidyData$code <- activities[TidyData$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.
codebook("*Appropriately labels the data set with descriptive variable names.")


names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

#Step 5: From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and each subject.

codebook("*From the data set in step 4, creates a second, independent tidy 
data set with the average of each variable for each activity and each subject.")


FinalData <- TidyData %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

# writing variable properties
codebook("") 
codebook("## `resultData` variable\n")
codebook("### key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
codebook("`subject`           | ID of subject, int (1-30)")
codebook("`activity_num`      | ID of activity, int (1-6)")
codebook("`activity_name`     | Label of activity, Factor w/ 6 levels")

codebook("### non-key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
codebook("`variable`          | comlete name of the feature, Factor w/ 66 levels (eg. tBodyAcc-mean()-X) ")
codebook("`value`             | the actual value, num (range: -1:1)")
codebook("`dimension`         | dimension of measurement, Factor w/ 2 levels: `t` (Time) or `f` (Frequency)")
codebook("`source`            | source of measurement, Factor w/ 3 levels: `Body`,`BodyBody` or `Gravity`")
codebook("`type`              | type of measurement, Factor w/ 2 levels: `Acc` (accelerometer) or `Gyro` (gyroscope)")
codebook("`jerk`              | is 'Jerk' signal , Factor w/ 2 levels:  `Jerk` or `` (non Jerk)")
codebook("`magnitude`         | is 'Magnitude' value , Factor w/ 2 levels:  `Mag` or `` (non Mag)")
codebook("`method`            | result from method , Factor w/ 2 levels:  `mean` (average) or `std` (standard deviation)")
codebook("`axis`              | FFT exrapolated to axis , Factor w/ 2 levels:  `` (no FFT-axis) or `X`, `Y` or `Z`")

codebook("") 
codebook("## `tidyData` variable\n")
codebook("### key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
codebook("`activity_name`     | Label of activity, Factor w/ 6 levels")
codebook("`subject`           | ID of subject, int (1-30)")


codebook("### non-key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
