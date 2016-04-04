##download the file
##download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "UCI_HAR.zip")
##unzip("UCI_HAR.zip")

## READING txt files
##training data set
mydata_tr_SET <- read.table("./UCI_HAR/train/X_train.txt",head=FALSE)
mydata_tr_ACT <- read.table("./UCI_HAR/train/Y_train.txt",head=FALSE)
mydata_tr_SUBJECT <- read.table("./UCI_HAR/train/subject_train.txt",head=FALSE)
##test data set
mydata_ts_SET <- read.table("./UCI_HAR/test/X_test.txt",head=FALSE)
mydata_ts_ACT <- read.table("./UCI_HAR/test/Y_test.txt",head=FALSE)
mydata_ts_SUBJECT <- read.table("./UCI_HAR/test/subject_test.txt",head=FALSE)
##features 
Features <- read.table("./UCI_HAR/features.txt",head=FALSE)
##activity labels
ActivityLabels <- read.table("./UCI_HAR/activity_labels.txt",head=FALSE)

##preparing training data set
##renaming data set column names 
names(mydata_tr_SET) <- Features$V2
##add activity & subject columns to data set
names(mydata_tr_ACT) <- c("activity")
mydata_tr_SET <- cbind(mydata_tr_SET,mydata_tr_ACT)
names(mydata_tr_SUBJECT) <- c("subject")
mydata_tr_SET <- cbind(mydata_tr_SET,mydata_tr_SUBJECT)


##Getting and preparing test data set
##renaming data set column names 
names(mydata_ts_SET) <- Features$V2
##add activity & subject column to data set
names(mydata_ts_ACT) <- c("activity")
mydata_ts_SET <- cbind(mydata_ts_SET,mydata_ts_ACT)
names(mydata_ts_SUBJECT) <- c("subject")
mydata_ts_SET <- cbind(mydata_ts_SET,mydata_ts_SUBJECT)

##Merges the training and the test sets into one data set
mydata_mr_SET <- rbind(mydata_tr_SET,mydata_ts_SET)

##Extracts only the measurements on the mean and standard deviation for each measurement.
mydata_mr_SET <- mydata_mr_SET[ grepl("-mean()",Features$V2,fixed = TRUE) |
                                grepl("-std()",Features$V2,fixed = TRUE) 
                              ]

##Uses descriptive activity names to name the activities in the data set
fActivityLabels <- factor(ActivityLabels$V2)

##Appropriately labels the data set with descriptive variable names.
ApprLabel <- function(x){
  res<-gsub("^t","time",x)
  res<-gsub("^f","freq",res)
  res<-gsub("Gyro","Gyroscope",res)
  res<-gsub("Mag","Magnitude",res)
  res<-gsub("Acc","Accelerometer",res)
  res<-gsub("\\(\\)","",res)
  res<-gsub("-","_",res)
  res
}
names(mydata_mr_SET) <- sapply(colnames(mydata_mr_SET),ApprLabel)

##creates independent tidy data set with the average of each variable for each activity and each subject.
mydata_Tidy_SET <-aggregate(. ~subject + activity,data= mydata_mr_SET , mean)

## write the tidy data set into txt file
if(!file.exists("dataset.txt")){file.create("dataset.txt")}
write.table(mydata_Tidy_SET,file="dataset.txt", row.name=FALSE)

## create the codebook
 if(!file.exists("CodeBook.md")){file.create("CodeBook.md")}
 Write(codebook(mydata_Tidy_SET),file="CodeBook.md")
 
 
 
 

