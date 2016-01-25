## Step 0:  Access the Data
## download the zip file, create (if it doesn’t exist) into the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
## unzip the downloaded file into the data folder
unzip(zipfile="./data/Dataset.zip",exdir="./data")
datapath <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(datapath, recursive=TRUE)
library(plyr);

## Activity values = y_test.txt and y_train.txt 
## Subjects values = subject_test.txt and subject_train.txt 
## Features values = features.txt
## Features values = x_test.txt and x_train.txt 
## Activity levels = activity_labels.txt

##read the Activity value files of y_test.txt and y_train.txt
ActivityTest  <- read.table(file.path(datapath, "test" , "Y_test.txt" ),header = FALSE)
ActivityTrain <- read.table(file.path(datapath, "train", "Y_train.txt"),header = FALSE)
##read the subject value files of subject_test.txt and subject_train.txt
SubjectTrain <- read.table(file.path(datapath, "train", "subject_train.txt"),header = FALSE)
SubjectTest  <- read.table(file.path(datapath, "test" , "subject_test.txt"),header = FALSE)
##read the features value files of x_test.txt and x_train.txt
FeaturesTest  <- read.table(file.path(datapath, "test" , "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(datapath, "train", "X_train.txt"),header = FALSE)

## Step 1:  Merges the training and the test sets to create one data set.
## merge the tables by rows
Subject <- rbind(SubjectTrain, SubjectTest)
Activity<- rbind(ActivityTrain, ActivityTest)
Features<- rbind(FeaturesTrain, FeaturesTest)
## give names to variables
setnames(Subject, "V1", "subject")
setnames(Activity, "V1", "activity") 
#read in the variable feature names table
FeaturesNames <- read.table(file.path(datapath, "features.txt"),head=FALSE)
names(Features)<- FeaturesNames$V2
##merge the columns to get the data frame Data for all data
Combine <- cbind(Subject, Activity)
Data <- cbind(Features, Combine)

## Step 2:  Extracts only the measurements on the mean and standard deviation for each measurement.
## subset Features Names by measurements on the mean and standard deviation
subFeaturesNames<-FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]
## create a subset of Data by selected names of Features
selectedNames<-c(as.character(subFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)

##Step 3:  Uses descriptive activity names to name the activities in the data set
## download activity_labels.txt to obtain the descriptive activity names
activityLabels <- read.table(file.path(datapath, "activity_labels.txt"),header = FALSE)

##Step 4:  Appropriately labels the data set with descriptive variable names
## use descriptive variable names for features
## prefix t -> Time
## prefix f -> Frequency
## Acc -> Accelerometer
## Gyro -> Gyroscope
## Mag -> Magnitude
## BodyBody -> Body
names(Data)<-gsub("^t", "Time", names(Data))
names(Data)<-gsub("^f", "Frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

##Step 5:  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

NewData<-aggregate(. ~subject + activity, Data, mean)
NewData <- NewData[order(NewData$subject, NewData$activity),]
write.table(NewData, file = "tidydata.txt",row.name=FALSE)
print(NewData)

