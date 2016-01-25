Getting and Cleaning Data Course Project 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 
1.	a tidy data set as described below
2.	a link to a Github repository with your script for performing the analysis
3.	a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 

You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article (http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/). Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

You should create one R script called run_analysis.R that does the following.
1.	Merges the training and the test sets to create one data set.
2.	Extracts only the measurements on the mean and standard deviation for each measurement.
3.	Uses descriptive activity names to name the activities in the data set
4.	Appropriately labels the data set with descriptive variable names.
5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Background
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
?	Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
?	Triaxial Angular velocity from the gyroscope. 
?	A 561-feature vector with time and frequency domain variables. 
?	Its activity label. 
?	An identifier of the subject who carried out the experiment.

The following files were created:
?	Activity from:  
o	y_train.txt for the activity of the subjects’ generated training data 
o	y_ test.txt for the activity of test data 
?	Subject from:
o	subject_train.txt of the subjects’ generated training data
o	subject_ test.txt of the test data
?	Features from:
o	x_train.txt of the subjects’ generated training data
o	x_test.txt of the subjects’ generated training data

The following files are also used:
?	variable names:  features.txt
?	levels of activity:  activity_levels.txt

## Step 0:  Access the Data
## download the zip file, create (if it doesn’t exist) into the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
##unzip the downloaded file into the data folder
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
#newly created tidy data set named NewData
NewData<-aggregate(. ~subject + activity, Data, mean)
NewData <- NewData [order(NewData$subject, NewData$activity),]
write.table(NewData, file = "tidydata.txt",row.name=FALSE)

