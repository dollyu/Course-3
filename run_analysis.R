##########################################
###       Coursera Course 3            ###
###                                    ###
###  Dorothea Ugi                      ###
###  28 OCT 2019                       ###
###  Program:  run_analysis.R          ###
##########################################

## You should create one R script called run_analysis.R that does the following.

## 1.) Merges the training and the test sets to create one data set.
## 2.) Extracts only the measurements on the mean and standard deviation for each measurement.
## 3.) Uses descriptive activity names to name the activities in the data set
## 4.) Appropriately labels the data set with descriptive variable names.
## 5.) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### loading packages
library("data.table")
library("reshape2")

### setting working directory ###
setwd("/home/dugi/coursera/Course3_week4")
path=getwd()

##### Retrieve data

### activity labels + features
	activityLabels <- fread(file.path(path, "activity_labels.txt"), col.names = c("classLabels", "activityName"))
	features <- fread(file.path(path, "features.txt"), col.names = c("index", "featureNames"))
	featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
	measurements <- features[featuresWanted, featureNames]
	measurements <- gsub('[()]', '', measurements)

### train datasets - reading and combining
	train <- fread(file.path(path, "X_train.txt"))[, featuresWanted, with = FALSE]
	data.table::setnames(train, colnames(train), measurements)
	trainActivities <- fread(file.path(path, "y_train.txt"), col.names = c("Activity"))
	trainSubjects <- fread(file.path(path, "subject_train.txt"), col.names = c("SubjectNum"))
	train <- cbind(trainSubjects, trainActivities, train)

### test datasets - reading and combining
	test <- fread(file.path(path, "X_test.txt"))[, featuresWanted, with = FALSE]
	data.table::setnames(test, colnames(test), measurements)
	testActivities <- fread(file.path(path, "y_test.txt"), col.names = c("Activity"))
	testSubjects <- fread(file.path(path, "subject_test.txt"), col.names = c("SubjectNum"))
	test <- cbind(testSubjects, testActivities, test)

### Merging datasets & adding labels
	alltidydata <- rbind(train, test)

### Making data tidy 
	alltidydata[["Activity"]] <- factor(alltidydata[, Activity]
                              , levels = activityLabels[["classLabels"]]
                              , labels = activityLabels[["activityName"]])

	alltidydata[["SubjectNum"]] <- as.factor(alltidydata[, SubjectNum])
	alltidydata <- reshape2::melt(data = alltidydata, id = c("SubjectNum", "Activity"))
	alltidydata <- reshape2::dcast(data = alltidydata, SubjectNum + Activity ~ variable, fun.aggregate = mean)

	write.table(x = alltidydata, file = "AllTidyData.txt", row.name=FALSE)