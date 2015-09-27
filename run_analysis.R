#Assignment:
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 
# Good luck!

#Inlcude library
library(dplyr)

#" " refers to one whitespace character. "" refers to any length whitespace as being the delimiter.

#read train data set
uci_train = read.csv('train\\X_train.txt',sep = "",header = FALSE)
uci_train_label = read.csv('train\\y_train.txt',sep = ' ',header = FALSE)
uci_train_subject = read.csv('train\\subject_train.txt',sep = ' ',header = FALSE)

#read test data set
uci_test  = read.csv('test\\X_test.txt',sep = "",header = FALSE)
uci_test_label = read.csv('test\\y_test.txt',sep = ' ',header = FALSE)
uci_test_subject = read.csv('test\\subject_test.txt',sep = ' ',header = FALSE)

#merge train and test data
uci_data = rbind(uci_train,uci_test)

#read descritions
features  = read.csv('features.txt',sep = "",header = FALSE)
activites = read.csv('activity_labels.txt',sep = "",header = FALSE)

#find column index that represent the mean and standard deviation for each measurement. 
ind_std  <- which(apply(features, 1, function(x) any(grepl("std", x))))
ind_mean <- which(apply(features, 1, function(x) any(grepl("mean", x))))
ind_all <- sort(c(row_std,row_mean))

#select found cloumns 
uci_data_sel <- uci_data[,ind_all]

#add activity and subject to data set
uci_data_sel$Activity <- activites[c(uci_train_label[[1]],uci_test_label[[1]]),][[2]]
uci_data_sel$Subject  <- c(uci_train_subject[[1]],uci_test_subject[[1]])

#create final result data set
result <- uci_data_sel %>%
  group_by(Activity,Subject) %>%
  summarise_each(funs(mean)) %>%           
  print

#save result
write.csv(result,'summary_result.txt',sep = ",",row.names = FALSE)

print('Thanks for review!')
