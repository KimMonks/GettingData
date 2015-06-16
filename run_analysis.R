# Load the data


filesPath <- "C:/Users/user/Desktop/Coursera/GettingData/Project/data/UCI HAR Dataset"
setwd(filesPath)

if(!file.exists(filesPath)){
  dir.create(filesPath)
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="C:/Users/user/Desktop/Coursera/Getting Data/Project/data/UCI HAR DataSet.zip")

  ###Unzip DataSet to /data directory
  unzip(zipfile="C:/Users/user/Desktop/Coursera/Getting Data/Project/data/UCI HAR DataSet.zip",exdir="C:/Users/user/Desktop/Coursera/Getting Data/Project/data")
}


# Read files

subjectTrain <- read.table("train/subject_train.txt")
xTrain <- read.table("train/x_train.txt")
yTrain <- read.table("train/y_train.txt")
subjectTest <- read.table("test/subject_test.txt")
xTest <- read.table("test/x_test.txt")
yTest <- read.table("test/y_test.txt")
# Nasme the columns
dataFeatures <- read.table("features.txt")
colnames(xTest) <- dataFeatures$V2
colnames(xTrain) <- dataFeatures$V2

#1.  Merges the training and the test sets to create one data set.
# Merge Rows
dataHAR <- rbind(xTrain, xTest)
subjectData <- rbind(subjectTrain, subjectTest)
activityData <- rbind(yTrain, yTest)

# Name columns and merge columns

colnames(subjectData) <- "subject"
colnames(activityData) <- "activityNum"
activityLabels<- read.table("activity_labels.txt")
colnames(activityLabels) <- c("activityNum", "activityNam")
dataSubjAct<- cbind(subjectData, activityData)
dataHAR <- cbind(dataSubjAct, dataHAR)


#2.  Extracts only the measurements on the mean and standard deviation for each measurement. 

# extract std and then mean colmns and column bind with subject and activity
datastdmean <- cbind(dataHAR[,grep("std", names(dataHAR))],dataHAR[,grep("mean", names(dataHAR))])
datastdmean <- cbind(dataSubjAct, datastdmean)

#3.  Uses descriptive activity names to name the activities in the data set
# Use the data set activity names for each a ctivity number
datastdmean <- merge(activityLabels, datastdmean , by="activityNum", all.x=TRUE)

#4.  Appropriately labels the data set with descriptive variable names.

# Use names provided

names(datastdmean)<-gsub("std()", "SD", names(datastdmean))
names(datastdmean)<-gsub("mean()", "MEAN", names(datastdmean))
names(datastdmean)<-gsub("^t", "time", names(datastdmean))
names(datastdmean)<-gsub("^f", "frequency", names(datastdmean))
names(datastdmean)<-gsub("Acc", "Accelerometer", names(datastdmean))
names(datastdmean)<-gsub("Gyro", "Gyroscope", names(datastdmean))
names(datastdmean)<-gsub("Mag", "Magnitude", names(datastdmean))
names(datastdmean)<-gsub("BodyBody", "Body", names(datastdmean))
## clean up column names
# get names
tidy.colnames <- names(datastdmean)
# remove non-alphabetic characters and convert to lowercase
tidy.colnames <- tolower(gsub("[^[:alpha:]]", "", tidy.colnames))
# then use the list as column names for data
colnames(datastdmean) <- tidy.colnames


write.table(datastdmean, "merged_HAR_data.txt") # write out the 1st dataset
#5.  From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

# find the mean for each combination of subject and activity
meanData <- aggregate(datastdmean[, 3:ncol(datastdmean)],
                       by=list(subject = datastdmean$subject, 
                               activity = datastdmean$activitynam),
                       mean)


# write the data for course upload
write.table(format(meanData, scientific=T), "tidy.txt",
            row.names=F, col.names=F, quote=2)

