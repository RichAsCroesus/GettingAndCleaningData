# run_analysis.R for Getting and Cleaning Data Project Summer 2014
#
#
# This script will perform the following operation
#     Merges the traing and test sets to create one data set.
#     Extracts only the measurements on the mean and standard deviation for each measurement 
#     Uses descriptive activity names to name the activities in the data set
#     Appropriately labels the data set with descriptive variable names
#     Creates a second, independent tidy data set with the average of each variable for each activity and each subject
#
#  Information files in the UCI HAR Dataset directory
#
#   The dataset includes the following files:
#     - 'README.txt'
#     - 'features_info.txt': Shows information about the variables used on the feature vector.
#     - 'features.txt': List of all features.
#     - 'activity_labels.txt': Links the class labels with their activity name.
#     - 'train/X_train.txt': Training set.
#     - 'train/y_train.txt': Training labels.
#     - 'test/X_test.txt': Test set.
#     - 'test/y_test.txt': Test labels.
#
#   README.txt (excerpt)
#     - Features are normalized and bounded within [-1,1].
#     - Each feature vector is a row on the text file.
#
#   activity_label.txt:
#         1 WALKING
#         2 WALKING_UPSTAIRS
#         3 WALKING_DOWNSTAIRS
#         4 SITTING
#         5 STANDING
#         6 LAYING
# features.txt
#         has all of the names we will need to prettyfy them

# read the x data
xtrain <-  read.table("./train/X_train.txt", header=FALSE)
xtest <- read.table("./test/X_test.txt", header=FALSE)

## read the y data thie is the activity in integer for maps with 
## the activities variable
ytrain <- read.table("./train/y_train.txt", header=FALSE)
ytest <-read.table("./test/y_test.txt", header=FALSE)

## read in the subject identifiers
subjectIdTrain <- read.table("./train/subject_train.txt")
subjectIdTest <- read.table("./test/subject_test.txt")

# merge the x, y and subject tables 
xall <- rbind(xtrain, xtest)

# yall is the activity (1-6)
yall <- rbind(ytrain, ytest)

# read in the activities table. 
activities <-read.table("./activity_labels.txt")
activityNames <- gsub("_", " ", tolower(as.character(activities$V2)))
activitiesNames <- c()
# create a column of activity names we will put them side-by-side in the front of the table
for ( i in yall ) {
  activitiesNames <- c(activitiesNames, activityNames[i])
}
# merge the subject train and test data 
subjectId <- rbind(subjectIdTrain, subjectIdTest)
foo <- yall
# add the y(activity) and subject ids to the front of the data frame xall
untidyTable <- cbind(subjectId, activitiesNames, yall, xall)

# get the feature names
features <- read.table("./features.txt", header=FALSE)
namesList <- features$V2

# clean the names
# remove special characters
namesList <- gsub("\\(", "", namesList)
namesList <- gsub("\\)", "", namesList)
namesList <- gsub("\\-", "", namesList)
namesList <- gsub("\\,", "", namesList)

# make the names a little clearer 
# change t prefix to time and f to freq
namesList <- gsub("tBody","timeBody", namesList)
namesList <- gsub("tGrav","timeGrav", namesList)
namesList <- gsub("fBody","freqBody", namesList)

# add subject id and activity to the names list
namesListFull <- c("subjectId", "activity", "activityIndex", namesList)
names(untidyTable) <- namesListFull

# any variable with mean or std in name will be culled from the merged data set
# need to case insensitive match
selectionVectorOnMeanAndStd <- grepl("mean|std", namesListFull, ignore.case=TRUE)
# have added 3 columns to the front of the data.frame that we want for the tiny data result
selectionVectorOnMeanAndStd[1] <- TRUE
selectionVectorOnMeanAndStd[2] <- TRUE
selectionVectorOnMeanAndStd[3] <- TRUE

#rm(yall, xall, features, subjectId, subjectIdTest, ytrain, ytest, xtrain, xtest)
# pick out only the variables we will put in the tidy data set
selectionTable <- untidyTable[selectionVectorOnMeanAndStd]


# create the tiny data set by averaging over subjectId and activityIndex

# first generate the sql query
sqlSelectPrefix <- "select subjectId, activity, activityIndex"
sqlSelectSuffix <- " from selectionTable group by subjectId, activityIndex"
averages <-""
columnNames <- names(selectionTable)
columnNames <- columnNames[4:length(columnNames)]
for ( name in columnNames ) {
  newAve <- paste( ",AVG(", name, ")", sep="") 
  averages <- paste(averages, newAve, sep="" )
}

# create the tinyData data frame
fullQuery <- paste(sqlSelectPrefix, averages, sqlSelectSuffix)
tidyData <- sqldf(fullQuery)

#fix some of the names in tiny data
tidyDataNames <- names(tidyData)
# the query that generates the tidy data creates the column names base on the select statement 
tidyDataNames <- gsub("\\(", " ", tidyDataNames)
tidyDataNames <- gsub("\\)", "", tidyDataNames)
write(tidyDataNames, file="./tidyDataNames")
# write the tinyData data frame to a file in the current working directory.  
write.csv(tidyData, "./tidyData.csv", row.names=FALSE, quote=FALSE)

