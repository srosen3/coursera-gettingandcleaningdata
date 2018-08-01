#===================================================================
# 1) Merges the training and the test sets to create one data set.
#===================================================================

filename<- "getdata.zip"

#===================================================================
# 1.1 Download and unzip dataset
#===================================================================
if(!file.exists(filename)) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method = "curl")
}

if(!file.exists("UCI HAR Dataset")){
    unzip(filename)
}   

#===================================================================
# 1.2 Read Data
#===================================================================
path <- "UCI HAR Dataset"
#Read train data
    trainingSubjects <- read.table(file.path(path, "train", "subject_train.txt"))   # vector of subject ID's
    trainingSet <- read.table(file.path(path, "train", "X_train.txt"))              # data set
    trainingLabels <- read.table(file.path(path, "train", "y_train.txt"))           # vector of activity labels

#read test data
    testSubjects <- read.table(file.path(path, "test", "subject_test.txt"))        #  
    testSet <- read.table(file.path(path, "test", "X_test.txt"))                   #
    testLabels <- read.table(file.path(path, "test", "y_test.txt"))                #

# read activities
    activities <- read.table(file.path(path, "activity_labels.txt"))
    colnames(activities) <- c("activity", "activityLabel")
#read features
    features <- read.table(file.path(path, "features.txt"),as.is = TRUE) #dont convert characters to factors
    class(features[,2]) #just to check
    
#===================================================================
# 1.3 Combine 
#===================================================================
    # a<-   cbind(trainingSubjects, trainingSet, trainingLabels) these create two nx563 tables, 
    # b<-   cbind(testSubjects, testSet, testLabels)  
    
    # combine tables and remove old tables
total <- rbind(
    cbind(trainingSubjects, trainingSet, trainingLabels),
    cbind(testSubjects, testSet, testLabels))#; rm(trainingSubjects, trainingSet, trainingLabels, testSubjects, testSet, testLabels)    
    
    #confirm correct ncol
nrow(features)
ncol(total)
colnames(total) <- c("subject", features[, 2], "activity")
    
#===================================================================
# 2 Extract Mean and SD 
#===================================================================    
    keep <- grepl("subject|activity|mean|std", colnames(total), ignore.case = FALSE)
    total <- total[, keep] #total goes from 563 columns to 81
    
    
#===================================================================
# 3 Use Descriptive Activity Names to Name Each Activities
#===================================================================     
    #total[,activity] <-> colnames(activities[,2])
     total<- merge(total, activities, by  = "activity")
    # >>>ALTERNATIVE<<< total$activity <- factor(total$activity, 
    #levels = activities[, 1], labels = activities[, 2])
    
#===================================================================
# 4 Appropriately label data sets
#===================================================================    
    
    totalCols <- colnames(total)
    totalCols
#remove weird ()-
    totalCols <- gsub("[\\(\\)-]", "", totalCols)

#clean names
        totalCols <- gsub("^t", "timeDomainSignals", totalCols)
        totalCols <- gsub("^f", "frequencyDomainSignals", totalCols)   
        totalCols <- gsub("Acc", "Accelerometer", totalCols)
        totalCols <- gsub("Gyro", "Gyroscope", totalCols)
        totalCols <- gsub("Mag", "Magnitude", totalCols)
        totalCols <- gsub("Freq", "Frequency", totalCols)
        totalCols <- gsub("std", "StandardDeviation", totalCols)
        totalCols <- gsub("mean", "Mean", totalCols)
        totalCols <- gsub("BodyBody", "Body", totalCols)

# use cleaned names as colnames
        colnames(total) <- totalCols
        
        
#===================================================================================================
# 5 Create 2nd Independent Tidy Dataset, with average of each variable for each activity and subject
#===================================================================================================     

        library(dplyr)
        # group by subject and activity and summarise using mean
        totalActivityMeans <- total %>% 
            group_by(subject, activity) %>%
            summarise_each(funs(mean))
        
        # output to file "tidy_data.txt"
        write.table(totalActivityMeans, "tidy_data.txt", row.names = FALSE, 
                    quote = FALSE)
        