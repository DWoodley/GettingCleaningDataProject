library(utils)
library(dplyr)
library(data.table)
library(stringr)

###############################################################################
## Change working directory
setwd("c:/datasciencecoursera/Getting and Cleaning Data/Course Project")


###############################################################################
## Set name of zip file to be downloaded and download into working directory
filename = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile = "HARDataset.zip"
download.file(filename,destfile=destFile,method="curl")


###############################################################################
## Unzip entire dataset into UCI HAR Dataset directory
fileDesc = "C:/datasciencecoursera/Getting and Cleaning Data/Course Project/HARDataset.zip"
unzip(fileDesc)
setwd("C:/datasciencecoursera/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

###############################################################################
#Read in activity_labels.txt; rename columns to "activity_id","activity"
activity_labels <- read.table("activity_labels.txt",stringsAsFactors = FALSE)
colnames(activity_labels) <- c("activity_id","activity")

###############################################################################
#Read in features.txt; rename columns to "column_id","feature_name"
features <- read.table("features.txt",stringsAsFactors = FALSE)
colnames(features) <- c("feature_id","feature_name")
features <- as.data.table(features)

# Create vector of column names
collist <- c("subject","activity_id",as.vector(features$feature_name))

###############################################################################
#Read in featureTranslations.txt; rename columns to "feature","name"
#This table was added to aid in translation of column names to readable names
featureTrans <- read.table("../featureTranslations.txt",sep=",",
                                  stringsAsFactors = FALSE)
colnames(featureTrans) <- c("feature","name")


###############################################################################
#Get list of column numbers for means and standard deviation variables
x <- features$feature_name %in% str_subset(features$feature_name,"mean")
y <- features$feature_name %in% str_subset(features$feature_name,"std")
colnums <- c(1,2,features[x]$feature_id+2,features[y]$feature_id+2)


################################################################################
### Read in training datasets
#Read in listing of subject by row number rename column to "subject_id"
subject_train <- read.table("train/subject_train.txt")

#Read in actual measurment data
X_train <- read.table("train/X_train.txt")

#Read in train/Y_train.txt; rename column to "activity_id"
Y_train <- read.table("train/y_train.txt")

training_set <- cbind(subject_train,Y_train,X_train)

colnames(training_set) <- collist




################################################################################
### Read in test datasets
#Read in listing of subject by row number rename column to "subject_id"
subject_test <- read.table("test/subject_test.txt")

#Read in actual measurment data
X_test <- read.table("test/X_test.txt")

#Read in test/Y_test.txt; rename column to "activity_id"
Y_test <- read.table("test/y_test.txt")

testing_set <- cbind(subject_test,Y_test,X_test)

colnames(testing_set) <- collist

###############################################################################
##Combine training and testing datasets
dataset <- as.data.table(rbind(training_set[,colnums],testing_set[,colnums]))
collist <- collist[colnums]
################################################################################
##Change the column names in features table the using featureTran table here
for(n in 1:length(featureTrans$feature)) {
    collist <- gsub(featureTrans[n,1],featureTrans[n,2],collist)
   
}

collist <- make.names(collist)
colnames(dataset) <- collist
################################################################################

################################################################################
## Add readable activity names
dataset$activity <- ""
for(n in activity_labels$activity_id) {
    dataset[dataset$activity_id==n,]$activity <- activity_labels[n,]$activity 
}

## Reorder columns
dataset <- dataset[,c(1,82,3:81)]

################################################################################
## Write "tidy" data to file
write.csv(dataset,"../TidyDataset.csv",quote=FALSE,row.names=FALSE)


###############################################################################
# Create table of averages by subject and activity
if(exists("crossmeans")) {
    rm("crossmeans")
}

for(sbj in unique(dataset$subject)) {
    for(acc in unique(dataset$activity)) {
        tmp <- as.list(colMeans(dataset[activity==acc & subject == sbj,3:length(dataset[1,])]))
        
        if(!exists("crossmeans")) {
            crossmeans <- as.data.frame(c(subject=sbj,activity=acc,tmp))
        }
        else {
            crossmeans <- rbind(crossmeans,as.data.frame(c(subject=sbj,activity=acc,tmp))) 
        }
    }
}

###############################################################################
## Clean up un-needed tables
rm("training_set","testing_set","X_test","Y_test","X_train","Y_train",
   "subject_test","subject_train","activity_labels","features","featureTrans",
   "tmp")
