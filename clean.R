
# Load data table data
train = read.table("D:\\temp\\train\\X_train.txt")
test = read.table("D:\\temp\\test\\X_test.txt")

# Merge Data

Sample = rbind(train,test)

# Load data table activity
activityTrain = read.table("D:\\temp\\train\\Y_train.txt")
activityTest = read.table("D:\\temp\\test\\Y_test.txt")

activity = rbind(activityTrain, activityTest)

#row mean

meanSample = rowMeans(Sample)

Sample$Mean = meanSample


#subjects

subjects = read.table("D:\\temp\\train\\subject_train.txt")

subjectsTest = read.table("D:\\temp\\test\\subject_test.txt")

subjects = rbind(subjects, subjectsTest)


stdSample = rep(NA, nrow(Sample))


# Std for each rows

for (j in 1:nrow(Sample))
{
	x = 0
	for (i in 1:561)
	{
		x = x + (Sample[j,i] - meanSample[i]) ^ 2
	}
	stdSample[j] = (x / 560) ^ 0.5
}


Sample$SubjectId = subjects

#Load activity labels (Step 3)

labels2  = read.table("D:\\temp\\activity_labels.txt")


# Apply Label to Code

Labels2 = labels[,2]


ActivityCode = factor(activity , labels= Labels2)

Sample$ActivityCode = ActivityCode


#Build Tidy Data

temp = matrix()
temp = cbind(temp,  data.frame(subjects))
temp = cbind(temp, data.frame(activity))
temp = cbind(temp, meanSample)
 colnames(temp) = c("2","SubjectId","ActivityInt", "Mean")

agg =aggregate(temp, by=list(temp$ActivityInt, temp$SubjectId), FUN=mean)

install.packages('reshape')
library(reshape)

t =cast(agg, SubjectId ~ ActivityInt)



