
setwd("C:/code/study/git/r-kaggle-titanic")
source("C:/code/study/git/r-kaggle-titanic/code/titanic_helper.R")

library(seqinr)
library(e1071)
library(party)
library(Amelia)
library(ggplot2)
library(corrgram)

set.seed(100)
#----------------------------- Read Data --------------------------------
# Adding na.strings makes the data munging faster
titanic.master.data <- read.table("data/titanic/train.csv",header = TRUE, sep=",", na.strings = c("NA", ""))
titanic.exploration.data<-titanic.master.data

titanic.evaluation.data <- read.table("data/titanic/test.csv",header = TRUE, sep=",", na.strings = c("NA", ""))
titanic.evaluation.data$Survived <- 0

#----- Split data into training vs  and test data set (10%) for cross validation ---------

titanic.master.data.indices <-seq(1:nrow(titanic.master.data))
titanic.master.test.data.indices<-sample(x = titanic.master.data.indices,size = nrow(titanic.master.data)/10)

titanic.master.test.data <-titanic.master.data[titanic.master.test.data.indices,]
titanic.master.training.data <- titanic.master.data[-titanic.master.test.data.indices,]


#------------ Temporary Train and Test assignment ---------------------
# This will be replaced/mutually exclusive with actual test data

# for cross Validation
titanic.training.data  <- titanic.master.training.data
titanic.test.data <- titanic.master.test.data

titanic.training.data  <- titanic.master.training.data
titanic.test.data <- titanic.evaluation.data
#----------------------- Feature clean up -------------------------------

titanic.training.data <- clean.features.one(titanic.data = titanic.training.data)
titanic.test.data <- clean.features.one(titanic.data = titanic.test.data)
titanic.test.data$Survived <-as.factor(titanic.test.data$Survived)
levels(titanic.test.data$Survived) <- levels(titanic.training.data$Survived)
#--------------------------------- Feature Generation Title -------------------------
titanic.training.data <- generate.new.feature.is.child(titanic.data = titanic.training.data)
titanic.test.data <- generate.new.feature.is.child(titanic.data = titanic.test.data)

titanic.training.data <- generate.new.feature.binned.fare(titanic.data = titanic.training.data)
titanic.test.data <- generate.new.feature.binned.fare(titanic.data = titanic.test.data)

titanic.training.data$family.size <- generate.new.feature.family.size(titanic.data = titanic.training.data)
titanic.test.data$family.size <- generate.new.feature.family.size(titanic.data = titanic.test.data)
#-----------------------------------Filter Features----------------------------
titanic.training.data <- filter.features.one(titanic.data = titanic.training.data)
titanic.test.data <- filter.features.one(titanic.data = titanic.test.data)

#Find the Title of the Passenger Mr, Miss
titanic.training.data$Title <- sapply(titanic.training.data$Name,FUN = title.extraction.function)
titanic.training.data$Title <- as.character(trimSpace(titanic.training.data$Title, leading = TRUE, trailing = TRUE))

titanic.test.data$Title <- sapply(titanic.test.data$Name,FUN = title.extraction.function)
titanic.test.data$Title <- as.character(trimSpace(titanic.test.data$Title, leading = TRUE, trailing = TRUE))


#--------------------------------- Insert Missing Age with a combined Data Frame -------------------------
#Aggregate, take mean and insert missing age -- The below can be done using the ddply package
titanic.combined.age.title.training.data <- subset(titanic.training.data,select = c('Title','Age'))
titanic.combined.age.title.test.data <- subset(titanic.test.data,select = c('Title','Age'))
titanic.combined.age.title.data <- rbind(titanic.combined.age.title.training.data,titanic.combined.age.title.test.data)

titanic.combined.age.title.data <- titanic.combined.age.title.data[!is.na(titanic.combined.age.title.data$Age),]
aggregate.age.per.title<-aggregate(formula = titanic.combined.age.title.data$Age~titanic.combined.age.title.data$Title,data = titanic.combined.age.title.data, FUN = mean)
aggregate.sum.per.title<-aggregate(formula = titanic.combined.age.title.data$Age~titanic.combined.age.title.data$Title,data = titanic.combined.age.title.data, FUN = sum)

names(aggregate.age.per.title) <- c('Title','MeanAge')
names(aggregate.sum.per.title) <- c('Title','SumAge')

aggregate.age.per.title$MeanAge <- round(aggregate.age.per.title$MeanAge)
aggregate.age.per.title$SumAge <- aggregate.sum.per.title$SumAge
aggregate.age.per.title$Count  <- round(aggregate.age.per.title$SumAge/aggregate.age.per.title$MeanAge)

## Replace mean ages in both test and training data
titanic.training.data$Age[is.na(titanic.training.data$Age)] <-sapply(titanic.training.data$Title[is.na(titanic.training.data$Age)],fetch.mean.age.for.title.function, aggregate.age.per.title = aggregate.age.per.title)
titanic.test.data$Age[is.na(titanic.test.data$Age)] <-sapply(titanic.test.data$Title[is.na(titanic.test.data$Age)],fetch.mean.age.for.title.function, aggregate.age.per.title = aggregate.age.per.title)

#Remove Unwanted Columns
titanic.training.data$Name<-NULL
titanic.test.data$Name<-NULL


titanic.training.data$Title <- NULL
titanic.test.data$Title <- NULL

#titanic.training.data$SibSp <- NULL
#titanic.test.data$SibSp <- NULL


titanic.training.data$Parch <- NULL
titanic.test.data$Parch <- NULL

titanic.training.data$family.size <- NULL
titanic.test.data$family.size <- NULL

#Introduce model.performance df for comparing models performances
model.performance <- data.frame(model=character(), hyperparameter=numeric(), accuracy=numeric(), row.names=NULL, stringsAsFactors = FALSE)





#------------------------ Attempt Random Forest -----------------------------------------
#Train The Data
accuracy.percentage <- NULL

titanic.cforest.model <- cforest(titanic.training.data$Survived ~ .,data = titanic.training.data)
titanic.cforest.model
#plot(titanic.cforest.model)

#Test Data
titanic.test.data$rforest.predicted.survived <-predict(titanic.cforest.model,newdata = titanic.test.data)

table(titanic.test.data$rforest.predicted.survived,titanic.test.data$Survived)

accuracy.percentage <- mean(with(titanic.test.data, rforest.predicted.survived==Survived))

model.performance <- rbind(model.performance,data.frame(model="Random Forest ",accuracy=accuracy.percentage))
model.performance

#------------------------ Attempt Logistic Regression -----------------------------------------
#Train The Data
accuracy.percentage <- NULL
titanic.logistic.model <- glm(formula = titanic.training.data$Survived ~ .,data = titanic.training.data, family = 'binomial')
titanic.logistic.model

#Test Data
titanic.test.data$logistic.predicted.survived <-ifelse(predict(titanic.logistic.model,newdata = titanic.test.data) < 0 ,0,1)
titanic.test.data$logistic.predicted.survived <- as.factor(titanic.test.data$logistic.predicted.survived)
head(titanic.test.data$logistic.predicted.survived)
table(titanic.test.data$logistic.predicted.survived,titanic.test.data$Survived)
accuracy.percentage <- mean(with(titanic.test.data, logistic.predicted.survived==Survived))


model.performance <- rbind(model.performance,data.frame(model="Logistic Regression ",accuracy=accuracy.percentage))

model.performance

#------------------------ Attempt SVM Radial-----------------------------------------
#Train The Data
accuracy.percentage <- NULL
titanic.svm.radial.basis.model <- svm(titanic.training.data$Survived ~ .,data = titanic.training.data, kernel = 'radial')
titanic.svm.radial.basis.model

#Test Data
titanic.test.data$svm.radial.predicted.survived <-predict(titanic.svm.radial.basis.model,newdata = titanic.test.data)

table(titanic.test.data$svm.radial.predicted.survived,titanic.test.data$Survived)
accuracy.percentage <- mean(with(titanic.test.data, svm.radial.predicted.survived==Survived))

model.performance <- rbind(model.performance,data.frame(model="SVM Radial ",accuracy=accuracy.percentage))


#------------------------ Attempt SVM Linear-----------------------------------------
#Train The Data
accuracy.percentage <- NULL
titanic.svm.linear.model <- svm(titanic.training.data$Survived ~ .,data = titanic.training.data, kernel = 'linear')
titanic.svm.linear.model

#Test Data
titanic.test.data$svm.linear.predicted.survived <-predict(titanic.svm.linear.model,newdata = titanic.test.data)

table(titanic.test.data$svm.linear.predicted.survived,titanic.test.data$Survived)
accuracy.percentage <- mean(with(titanic.test.data, svm.linear.predicted.survived==Survived))

model.performance <- rbind(model.performance,data.frame(model="SVM Linear ",accuracy=accuracy.percentage))

#-------- Bag Results ---------------------------
accuracy.percentage <- NULL
titanic.test.data$bagged.predicted.survived <- NULL
for(i in 1:nrow(titanic.test.data))
{
  bagged.mean<- (as.numeric(titanic.test.data$rforest.predicted.survived[i])+
                   as.numeric(titanic.test.data$svm.radial.predicted.survived[i])+
                   as.numeric(titanic.test.data$logistic.predicted.survived[i])+
                   as.numeric(titanic.test.data$svm.linear.predicted.survived[i]))/4.0
  
  print(bagged.mean)
  if(bagged.mean > 1.5)
  {
    titanic.test.data$bagged.predicted.survived[i] <- 1 
  }
  
  if(bagged.mean < 1.5)
  {
    titanic.test.data$bagged.predicted.survived[i] <- 0 
  }
  if(bagged.mean == 1.5)
  {
    titanic.test.data$bagged.predicted.survived[i] <- as.numeric(titanic.test.data$logistic.predicted.survived[i]) - 1
  }
}

titanic.test.data$bagged.predicted.survived <- as.factor(titanic.test.data$bagged.predicted.survived)
accuracy.percentage <- mean(with(titanic.test.data, bagged.predicted.survived==Survived))
model.performance <- rbind(model.performance,data.frame(model="Baggedr ",accuracy=accuracy.percentage))

#----- Result creation
titanic.result.df <- titanic.evaluation.data
titanic.result.df$Survived <- titanic.test.data$bagged.predicted.survived
titanic.result.df <- subset(titanic.result.df, select = c('PassengerId','Survived'))
write.table(x = titanic.result.df, file ="data/titanic/result_bagged.csv", sep="," ,append = FALSE,quote = FALSE, row.names = FALSE)

