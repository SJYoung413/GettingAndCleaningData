data_Description<- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"
data_Url<-" https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
sessionInfo()
#download.file(data_Url, destfile = "data.zip")
#unzip

#Question 1
setwd("~/Coursera/GettingAndCleaningData")
library(readr)
library(dplyr)
activity_labels<-read_table("activity_labels.txt", col_names = FALSE, col_types = cols(X1 = col_character()))
features<-read_table("features.txt",col_names = FALSE)

#################################Read Test Dataset ###############################
y_test<-read_table("test/y_test.txt", col_names = FALSE, col_types =  cols(X1 = col_character()))
y_test$X1.1<-y_test$X1
y_test$X1<-NULL
X_test<-read_table("test/X_test.txt", col_names = FALSE)
subject_test<-read_table("test/subject_test.txt", col_names = FALSE)


#Combine Dataset
test<-cbind(subject_test,y_test, X_test)

################################ TEST END #################################

######################## Read Train Dataset ##################################
y_train<-read_table("train/y_train.txt", col_names = FALSE, col_types =  cols(X1 = col_character()))
y_train$X1.1<-y_train$X1
y_train$X1<-NULL
X_train<-read_table("train/X_train.txt", col_names = FALSE)
subject_train<-read_table("train/subject_train.txt", col_names = FALSE)


#Combine Dataset
train<-cbind(subject_train,y_train, X_train)

################################ TRAIN END #################################

#Merge Test and Train
data<-rbind(test, train)
            
#2 Extracts only the measuresments on the mean and standard deviation for each measurement
#mean(): Mean value
#std(): Standard Deviation
#y-1
#X_test - 561
Column_Names<- c("subject", "activity", as.character(features$X2))
Mean_Std_Columns_Results<-grep("subject|activity|[Mm]ean|std", Column_Names, value = FALSE)
Final_Dataset<-data[,Mean_Std_Columns_Results]

#3 Uses descriptive activity names to name the activities in the dataset
names(activity_labels)<- c("Activity_Number", "Activity_Name")
# Change colname of one column
colnames(Final_Dataset)[colnames(Final_Dataset) == "X1.1"] <- "Activity_Number"
Final_Dataset<-merge(Final_Dataset, activity_labels, by = "Activity_Number")
Final_Dataset$Activity_Number<-NULL
#4 Appropriately lables the data set with Descriptive variables names.

Replace_Column_Names<-Column_Names[Mean_Std_Columns_Results]
#Std
Replace_Column_Names<-gsub("std","Std",Replace_Column_Names)
#Mean
Replace_Column_Names<-gsub("mean","Mean",Replace_Column_Names)
#Frequency
Replace_Column_Names<-gsub("^f","frequency",Replace_Column_Names)
# Time
Replace_Column_Names<-gsub("^t","time",Replace_Column_Names)
#AngleTime
Replace_Column_Names<-gsub("^anglet","angleTime",Replace_Column_Names)
#Punctuation
Replace_Column_Names<-gsub("[[:punct:]]","",Replace_Column_Names)

#Gravity
Replace_Column_Names<-gsub("Gravity","gravity",Replace_Column_Names)

#Apply new names to dataframe
names(Final_Dataset)<-Replace_Column_Names

#5) From the dataset in step 4, create a second, independent tidy dataset with the average of each variable for each activity and each subject.

Final_tidy_Dataset<- Final_Dataset %>% group_by(activity, subject) %>%
  summarise_all(funs(mean))

write.table(Final_tidy_Dataset, file = "Tidy_Dataset_Final.txt", row.names =  FALSE)
