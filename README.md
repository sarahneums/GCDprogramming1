# GCDprogramming1
##Purpose:
#extract data from different data sets merge, extract means/std dev, 
#then show the averages of data in a simplified tidy data set.

#extract test data
xtest<-read.table("~/School/1Spring15/Math378/UCI HAR Dataset/test/X_test.txt", quote="\"")
ytest<-read.table("~/School/1Spring15/Math378/UCI HAR Dataset/test/Y_test.txt", quote="\"")

#extract trains
xtrain<-read.table("~/School/1Spring15/Math378/UCI HAR Dataset/train/X_train.txt", quote="\"")
ytrain<-read.table("~/School/1Spring15/Math378/UCI HAR Dataset/train/y_train.txt", quote="\"")


#merge data
xtotal<-rbind(xtrain,xtest)
ytotal<-rbind(ytrain,ytest)

total<-cbind(xtotal,ytotal)

col_names<-read.table("~/School/1Spring15/Math378/UCI HAR Dataset/features.txt", quote="\"") #extract column names
colnames(total)<-t(col_names[,2]) #set the column names

#read in subject values for test
subject_test<- read.table("~/School/1Spring15/Math378/UCI HAR Dataset/test/subject_test.txt",quote="\"")

#read in subject values for train
subject_train<-read.table("~/School/1Spring15/Math378/UCI HAR Dataset/train/subject_train.txt",quote="\"")

#combine subjects
subject_total<-rbind(subject_train,subject_test)

#extract activity labels
activity_labels<- read.table("~/School/1Spring15/Math378/UCI HAR Dataset/activity_labels.txt",quote="\"")

#extracts only the row numbers with either mean or std dev
stdcount<-grep("std()",colnames(total))
meancount<-grep("mean()",colnames(total))

#saves the std and mean names into a new var
stdcols<-total[,stdcount]
meancols<-total[,meancount]

#removes mean frequency
meanFreq_cols<-grep("meanFreq",colnames(meancols))
mean_cols<-meancols[,-meanFreq_cols]

#combine mean cols, std cols, the subjects, and the activities
data<-cbind(subject_total,ytotal,meancols,stdcols)

#create tidydata data frame for later use
tidydata<-c()
subjects<-c(1:30)

#rename data col names
colnames(data)[1]<-"Subjects"
colnames(data)[2]<-"Activities"


#goes through ever person, every activity, adds to tidydata data frame
for(i in 1:30){
  for(j in 1:6){
    fake<-subset(data,Subjects==subjects[i]&Activities==activity_labels[j,1])
    vector<-apply(fake[,3:68],2,mean)
    new_vector<-cbind(subjects[i],activity_labels[j,2],t(vector))
    tidydata<-rbind(tidydata,new_vector)
  }
}


#rename cols in tidydata
colnames(tidydata)[1]="Subject"
colnames(tidydata)[2]="Activity"


#renames data points with easier to understand names
names(tidydata)<-gsub("At","Time",names(tidydata))
names(tidydata)<-gsub("Acc","Accelerometer",names(tidydata))
names(tidydata)<-gsub("^f","Frequency",names(tidydata))
names(tidydata)<-gsub("Gyro","Gyroscope",names(tidydata))
names(tidydata)<-gsub("Mag","Magnitude",names(tidydata))
names(tidydata)<-gsub("BodyBody","Body",names(tidydata))

#read tidydata matrix
write.table(tidydata,file="tidydata.txt",row.name=F)
