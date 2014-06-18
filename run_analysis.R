##### Step 1 
# read csv from train and test directory
trainx <- read.table("UCI HAR Dataset/train/X_train.txt", header=F)
testx <- read.table("UCI HAR Dataset/test/X_test.txt", header=F)
# test data after train data
mergex <- rbind(trainx, testx)

##### Step 2
# column number for measurements on mean and std
colmeanstd <- c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,
                85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,
                214,215,227,228,240,241,253,254,266,267,268,269,270,271,345,346,
                347,348,349,350,424,425,426,427,428,429,503,504,516,517,529,530,542,543)
# extract
submergex <- mergex[,colmeanstd]

##### Step 3 and 4
# read activity kind
trainy <- read.table("UCI HAR Dataset/train/y_train.txt", header=F)
testy <- read.table("UCI HAR Dataset/test/y_test.txt", header=F)
# test data after train data
mergey <- rbind(trainy, testy)

# read activity label
actlabel <- read.table("UCI HAR Dataset/activity_labels.txt", header=F)
# mapping activity kind to activity label
mergey$V1 <- factor(mergey$V1, levels=actlabel$V1, labels=actlabel$V2)

# column label for measurements on mean and std
collabel <- c("tBodyAcc-mean()-X","tBodyAcc-mean()-Y",
              "tBodyAcc-mean()-Z","tBodyAcc-std()-X",
              "tBodyAcc-std()-Y","tBodyAcc-std()-Z",
              "tGravityAcc-mean()-X","tGravityAcc-mean()-Y",
              "tGravityAcc-mean()-Z","tGravityAcc-std()-X",
              "tGravityAcc-std()-Y","tGravityAcc-std()-Z",
              "tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y",
              "tBodyAccJerk-mean()-Z","tBodyAccJerk-std()-X",
              "tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z",
              "tBodyGyro-mean()-X","tBodyGyro-mean()-Y",
              "tBodyGyro-mean()-Z","tBodyGyro-std()-X",
              "tBodyGyro-std()-Y","tBodyGyro-std()-Z",
              "tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y",
              "tBodyGyroJerk-mean()-Z","tBodyGyroJerk-std()-X",
              "tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z",
              "tBodyAccMag-mean()","tBodyAccMag-std()",
              "tGravityAccMag-mean()","tGravityAccMag-std()",
              "tBodyAccJerkMag-mean()","tBodyAccJerkMag-std()",
              "tBodyGyroMag-mean()","tBodyGyroMag-std()",
              "tBodyGyroJerkMag-mean()","tBodyGyroJerkMag-std()",
              "fBodyAcc-mean()-X","fBodyAcc-mean()-Y",
              "fBodyAcc-mean()-Z","fBodyAcc-std()-X",
              "fBodyAcc-std()-Y","fBodyAcc-std()-Z",
              "fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y",
              "fBodyAccJerk-mean()-Z","fBodyAccJerk-std()-X",
              "fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z",
              "fBodyGyro-mean()-X","fBodyGyro-mean()-Y",
              "fBodyGyro-mean()-Z","fBodyGyro-std()-X",
              "fBodyGyro-std()-Y","fBodyGyro-std()-Z",
              "fBodyAccMag-mean()","fBodyAccMag-std()",
              "fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-std()",
              "fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-std()",
              "fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-std()","activity")

# add activity label
mergexy <- cbind(submergex, mergey)

# descriptive column names
colnames(mergexy) <- collabel

# output tidy data set
write.table(mergexy, "./result_of_step4.txt")

##### Step 5

# read subject
trainsubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header=F)
testsubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header=F)
# test data after train data
merges <- rbind(trainsubject, testsubject)
colnames(merges) <- c("subject")
# add subject columns
mergexys <- cbind(mergexy, merges)
mergexys$subject = as.factor(mergexys$subject)

# concat activity and subject for factor
as <- paste(mergexys$activity, mergexys$subject)
mergexys$activity_list <- as.factor(as)

uas <- unique(as)

# firstly, set activity and subject columns 
activity <- c()
subject <- c()
for (u in uas) {
        t <- strsplit(u, " ")
        activity <- append(activity, t[[1]][1])
        subject <- append(subject, t[[1]][2])
}        
result <- data.frame(activity = activity, subject = subject)

colnames_of_result <- colnames(result)

# processing for each column
for (i in 1:66) {
        # get i'th column and activiy_list column
        icol <- mergexys[,c(i,69)]
        colnames_of_result <- append(colnames_of_result, colnames(mergexys)[i])
        tcol <- c()
        # each activity and subject
        for (u in uas) {
                m <- mean(icol[icol$activity_list == u,1])
                tcol <- append(tcol, m)
        }
        result <- cbind(result, tcol)
}
colnames(result) <- colnames_of_result
write.table(result, "./result_of_step5.txt")
