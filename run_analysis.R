library(reshape2)

###############################################################################
# Step 0. Get data and load activity labels and features
###############################################################################
filename <- "getdata_dataset.zip"

# Download and unzip the dataset:
if (!file.exists(filename)){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
activity_labels[,2] <- as.character(activity_labels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])


###############################################################################
# Step 1. Extracts only the measurements on the mean and standard deviation 
#		  for each measurement.
###############################################################################
features_desired_idx <- grep(".*mean.*|.*std.*", features[,2])
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")[features_desired_idx]
y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")[features_desired_idx]
y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")


###############################################################################
# Step 2. Merges the training and the test sets to create one data set.
###############################################################################
test <- cbind(subject_test, y_test, x_test)
train <- cbind(subject_train, y_train, x_train)
all_data <- rbind(train, test)


###############################################################################
# Step 3. Appropriately labels the data set with descriptive variable names.
###############################################################################
# turn activities & subjects into factors
features_desired_names <- features[features_desired_idx,2]
features_desired_names = gsub('-mean', 'Mean', features_desired_names)
features_desired_names = gsub('-std', 'Std', features_desired_names)
features_desired_names <- gsub('[-,()]', '', features_desired_names)
colnames(all_data) <- c("subject", "activity", features_desired_names)


###############################################################################
# Step 4. Uses descriptive activity names to name the activities in the data set
###############################################################################
all_data$activity <- factor(all_data$activity, levels = activity_labels[,1], labels = activity_labels[,2])
all_data$subject <- as.factor(all_data$subject)


###############################################################################
# Step 5. From the data set in previous step, creates a second, independent tidy data 
#		  set with the average of each variable for each activity and each 
#		  subject.
###############################################################################
melted_data <- melt(result, id = c("subject", "activity"))
result <- dcast(melted_data, subject + activity ~ variable, mean)


###############################################################################
# Step 6. Write tidy dataset into the file - result.txt.
###############################################################################
write.table(result, "result.txt", row.names = FALSE, quote = FALSE)



