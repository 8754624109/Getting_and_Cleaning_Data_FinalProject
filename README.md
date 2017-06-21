# the below analysis takes a dataset and prepares the set for future analysis. 

#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Here are the data for the project:

#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#I created a script called run_analysis.R that does the following.

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each #subject.

#loading packages

library(data.table)
library(dplyr)
library(reshape2)
install.packages("downloader")
library(downloader)

#for creating codebook
install.packages("memisc")
library(memisc)

#for creating markdown file
install.packages("rmarkdown")
library(rmarkdown)



#setting working directory
setwd("~/Documents/Data Analytics : Stats/R Training/COURSERA/Getting and Cleaning Data")

#setting subdirectory
if(!file.exists("./finalproject")){dir.create("./finalproject")}
setwd("~/Documents/Data Analytics : Stats/R Training/COURSERA/Getting and Cleaning Data/finalproject")

#set wd path
finalpath <- getwd()


#getting the data
projecturl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
finalzip <- "Dataset.zip"

download.file(projecturl, file.path(finalpath, finalzip))

#unzipping the zip folder
unzip("Dataset.zip", exdir = "./")

#unzipped file folder called "UCI HAR Dataset"
#set a new path for the UCI HAR Dataset
analysispath <- file.path(finalpath, "UCI HAR Dataset")
list.files(analysispath, recursive = TRUE)

#reading sbuject files
DTsubtrain <- fread(file.path(analysispath, "train", "subject_train.txt"))
DTsubtest <- fread(file.path(analysispath, "test", "subject_test.txt"))

#reading activity files
DTactivitytrain <- fread(file.path(analysispath, "train", "Y_train.txt"))
DTactivitytest <- fread(file.path(analysispath, "test", "Y_test.txt"))

#reading data files
dtTrainData <- data.table(read.table(file.path(analysispath, "train", "X_train.txt")))
dtTestData <- data.table(read.table(file.path(analysispath, "test", "X_test.txt")))

#merging the data sets
subjectdata <- rbind(DTsubtrain, DTsubtest)
setnames(subjectdata, "V1", "subject")

activitydata <- rbind(DTactivitytrain, DTactivitytest)
setnames(activitydata, "V1", "activityNum")

finaldt <- rbind(dtTrainData, dtTestData)

dtSubjectData <- cbind(subjectdata, activitydata)
finaldt <- cbind(dtSubjectData, finaldt)

View(finaldt)

#setting primary keys
setkey(finaldt, subject, activityNum)

#extracting mean and standard deviation
finaldtfeatures <- fread(file.path(analysispath, "features.txt"))
setnames(finaldtfeatures, names(finaldtfeatures), c("featureNum", "featureName"))

finaldtfeatures <- finaldtfeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

finaldtfeatures$featurecode <- finaldtfeatures[, paste0("V", featureNum)]
head(finaldtfeatures)

finaldtfeatures$featurecode

select <- c(key(finaldt), finaldtfeatures$featurecode)
select
finaldt <- finaldt[, select, with = FALSE]

finaldt

#descriptive activity names
dtActivityNames <- fread(file.path(analysispath, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#merge activity label

finaldt <- merge(finaldt, dtActivityNames, by = "activityNum", all.x = TRUE)

setkey(finaldt, subject, activityNum, activityName)

finaldt <- data.table(melt(finaldt, key(finaldt), variable.name = "featurecode"))

View(finaldt)

#merge activity name
finaldt <- merge(finaldt, finaldtfeatures[, list(featureNum, featurecode, featureName)], by = "featurecode", all.x = TRUE)

#create new activity factor variable
finaldt$activity <- factor(finaldt$activityName)
finaldt$feature <- factor(finaldt$featureName)

#create a tidy data set
View(finaldt)
as.numeric(finaldt$value)
as.character(finaldt$subject)
n_distinct(finaldt$subject)

library(dplyr)
finaldtgrouptidy <- group_by(finaldt, subject, activity, feature) %>% 
  summarise(average = mean(value))

write.table(finaldtgrouptidy, file = "finaldtgrouptidy", row.names = FALSE)


#generate codebook
finalproject_codebook <- codebook(finaldt)

#create txt file for codebook
write.table(finalproject_codebook, file = "finalproject_codebook", row.names = FALSE)
















