# Coursera/JHU Data Science Getting and Cleaning Data Course Project
# "Human Activity Recognition Using Smartphones" dataset

# Downloading data from the Url and checking required R packages
# and install them if not previously present
# ------------------------------------------------------------------------------
# 
# 
if("downloader" %in% rownames(installed.packages()) == FALSE) {install.packages("downloader")};library(downloader)
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")};library(data.table)
if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")};library(reshape2)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)


#############
#############

# Download data an unzip
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset")) {
  }
download(fileUrl, dest="data/UCI HAR Dataset.zip", mode="wb") 
unzip ("data/UCI HAR Dataset.zip")



# You should create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set.
# ------------------------------------------------------------------------------
# Step 1 - Merges the training and the test sets to create one data set.
# ------------------------------------------------------------------------------
## Goals
## 1. each variable should be in one column
## 2. each observation of that variable should be in a different row
## 3. include ids to link tables together

# training data


train_data <- cbind(cbind(x=read.table("UCI HAR Dataset/train/X_train.txt"),
                          subject=read.table("UCI HAR Dataset/train/subject_train.txt"))
                    , y=read.table("UCI HAR Dataset/train/y_train.txt"))

# test data
test_data <- cbind(cbind(x=read.table("UCI HAR Dataset/test/X_test.txt"),
                          subject=read.table("UCI HAR Dataset/test/subject_test.txt"))
                    , y=read.table("UCI HAR Dataset/test/y_test.txt"))

# Merging data

merged_data <- rbind(train_data, test_data)
features <- read.table("UCI HAR Dataset/features.txt", colClasses = c("character"))
merged_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(merged_data) <- merged_labels
rm(test_data,train_data)  # cleaning


# ------------------------------------------------------------------------------
# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# ------------------------------------------------------------------------------
# read features
small_data <- merged_data[,grepl("mean|std|Subject|ActivityId", names(merged_data))]


# ------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("ActivityId", "Activity"))
id_labels   = c("Subject", "ActivityId", "Activity")
data_labels = setdiff(colnames(small_data), id_labels)
for (i in 1:nrow(activity_labels)) {
  small_data$Activity[small_data$ActivityId == activity_labels[i, "ActivityId"]] <- as.character(activity_labels[i, "Activity"])
}

melt_data = melt(small_data, id = id_labels, measure.vars = data_labels)





# ------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.
# ------------------------------------------------------------------------------
names(small_data)
names(small_data) <- gsub("\\(|\\)", "", names(small_data))
names(small_data) <- gsub("Acc", "-acceleration", names(small_data))
names(small_data) <- gsub("Mag", "-Magnitude", names(small_data))
names(small_data) <- gsub("^t(.*)$", "\\1-time", names(small_data))
names(small_data) <- gsub("^f(.*)$", "\\1-frequency", names(small_data))
names(small_data) <- gsub("(Jerk|Gyro)", "-\\1", names(small_data))
names(small_data) <- gsub("BodyBody", "Body", names(small_data))
names(small_data) <- tolower(names(small_data))

## Appropriately labels the data set with descriptive activity names.




# ------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each
# ------------------------------------------------------------------------------

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, Subject + Activity ~ variable, mean)
write.table(tidy_data, file = "./tidy_data.txt")


#############
#############

  

