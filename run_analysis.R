# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# This R script does the following:

# 1. Merges the training and the test sets to create one data set.


t1<-read.table("UCI HAR Dataset/test/X_test.txt")
t2<-read.table("UCI HAR Dataset/train/X_train.txt")
t<-rbind(t1,t2)

s1<-read.table("UCI HAR Dataset/test/y_test.txt")
s2<-read.table("UCI HAR Dataset/train/y_train.txt")
s<-rbind(s1,s2)


p1<-read.table("UCI HAR Dataset/test/subject_test.txt")
p2<-read.table("UCI HAR Dataset/train/subject_train.txt")
p<-rbind(p1,p2)


# 2.Extracts only the measurements on the mean and standard deviation for each measurement.  

f<-read.table("UCI HAR Dataset/features.txt")
rf<-grep("-mean\\(\\)|-std\\(\\)",f[,2])
t <- t[,rf]
names(t) <- features[rf, 2]
names(t) <- gsub("\\(|\\)", "", names(t))
names(t) <- tolower(names(t))


# 3.Uses descriptive activity names to name the activities in the data set

act<-read.table("UCI HAR Dataset/activity_labels.txt")
act[,2]<-gsub("_","",tolower(as.character(act[,2])))
s[,1] <- act[s[,1], 2]
names(s) <- "activity"


# 4. Appropriately labels the data set with descriptive variable names.
names(p) <- "subject"
cleaned <- cbind(p, s, t)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects<-unique(p)[,1]
numSubjects<-length(unique(p)[,1])
numActivities<-length(act[,1])
numCols<-dim(cleaned)[2]
result<-cleaned[1:(numSubjects*numActivities), ]

row = 1
for (p in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[p]
        result[row, 2] = act[a, 2]
        tmp <- cleaned[cleaned$subject==p & cleaned$activity==act[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt")