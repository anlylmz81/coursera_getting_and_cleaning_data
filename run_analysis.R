library(data.table)
library(dplyr)

subjtest<- read.table("./test/subject_test.txt")
xtest<- read.table("./test/X_test.txt")
ytest<- read.table("./test/y_test.txt")

subjtrain<- read.table("./train/subject_train.txt")
xtrain<- read.table("./train/X_train.txt")
ytrain<- read.table("./train/y_train.txt")

features <- read.table("./features.txt")
activitylabels <- read.table("./activity_labels.txt")

## STEP1. Merge the training and the test sets to create one data set.

subject <- rbind(subjtest, subjtrain)
x <- rbind(xtrain,xtest)
y <- rbind(ytrain,ytest)
colnames(x) <- t(features[2])
colnames(y) <- "Activity"
colnames(subject) <- "Subject"
data<- cbind(x,y,subject)

##STEP2. Extract only the measurements on the mean and standard deviation for each measurement.

column <- grep("mean\\(\\)|std\\(\\)",names(data),ignore.case=TRUE)
columnall <- c(column,562,563)
datanew <- data[,columnall]

##STEP3. Use descriptive activity names to name the activities in the data set.

datanew$Activity <- as.character(datanew$Activity)
for (i in 1:6){
    datanew$Activity[datanew$Activity == i] <- as.character(activitylabels[i,2])
}
datanew$Activity <- as.factor(datanew$Activity)

##STEP4. Appropriately label the data set with descriptive variable names.

names(datanew) <- gsub("Acc", "Accelerometer", names(datanew))
names(datanew) <- gsub("Gyro", "Gyroscope", names(datanew))
names(datanew) <- gsub("Mag", "Magnitude", names(datanew))
names(datanew) <- gsub("^t", "Time", names(datanew))
names(datanew) <- gsub("^f", "Frequency", names(datanew))
names(datanew) <- gsub("tBody", "TimeBody", names(datanew))
names(datanew) <- gsub("-mean()", "Mean", names(datanew), ignore.case = TRUE)
names(datanew) <- gsub("-std()", "STD", names(datanew), ignore.case = TRUE)
names(datanew) <- gsub("-freq()", "Frequency", names(datanew), ignore.case = TRUE)
names(datanew) <- gsub("angle", "Angle", names(datanew))
names(datanew) <- gsub("gravity", "Gravity", names(datanew))

##STEP5. From the data set created above, create a second, independent tidy data set with the 
##average of each variable for each activity and each subject.
datanew$Subject <- as.factor(datanew$Subject)
datanew <- data.table(datanew)
tidydata <- aggregate(. ~Subject + Activity, datanew, mean)
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(tidydata, file = "Tidy.txt", row.names = FALSE)










