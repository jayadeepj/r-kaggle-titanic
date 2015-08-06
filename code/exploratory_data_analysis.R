
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

#-------------------------- exploratory data analysis .----------------------
#This is done on a copy of the master data  only for exploration 

# Correct The Fewture Types 
titanic.exploration.data <- clean.features.one(titanic.data = titanic.exploration.data)

# Check the type of features
lapply(titanic.exploration.data, class)

## Find missing data
missmap(titanic.exploration.data, legend = TRUE, col = c("yellow","black"), main = "Titanic Missing Data Map",)
# Lot of cabin missing , Some age missing

#Check the relation of survived vs died
table(titanic.exploration.data$Survived)

ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Survived))
ggplot.relation.object <-ggplot.relation.object+geom_bar()+ggtitle("Survived Bar Chart")
ggsave(filename ="plots/titanic/survived_vs_died.pdf" , plot=ggplot.relation.object)
ggplot.relation.object

#Check the PClass Distribution bar chart
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Pclass))
ggplot.relation.object <-ggplot.relation.object+geom_bar()+ggtitle("PClass Bar Chart")
ggsave(filename ="plots/titanic/PClass_Distribution_Bar.pdf" , plot=ggplot.relation.object)
ggplot.relation.object

# Sex vs Survived tabulation
table(titanic.exploration.data$Sex,titanic.exploration.data$Survived)
prop.table(table(titanic.exploration.data$Sex,titanic.exploration.data$Survived),1)

# Sex vs Survived bar chart
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Survived, fill = titanic.exploration.data$Sex))
ggplot.relation.object <-ggplot.relation.object+geom_bar()+ggtitle("Survived Bar Chart")
ggsave(filename ="plots/titanic/survival_vs_sex.pdf" , plot=ggplot.relation.object)
ggplot.relation.object

# Age distribution table
summary(titanic.exploration.data$Age)
plot(titanic.exploration.data$Age)

# Age distribution Histogram 
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Age))
ggplot.relation.object <-ggplot.relation.object+geom_bar(colour="darkred", fill="white")+ggtitle("Age Histogram Chart")
ggsave(filename ="plots/titanic/Age_Distribution.pdf" , plot=ggplot.relation.object)
ggplot.relation.object

# Embarked vs survival
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Survived,y = titanic.exploration.data$Embarked))
ggplot.relation.object <-ggplot.relation.object+geom_boxplot()+ggtitle("Survival vs Embarked Histogram Chart")
ggsave(filename ="plots/titanic/Embarked_Vs_Survival.pdf" , plot=ggplot.relation.object)
ggplot.relation.object


# Age vs survival box
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Survived,y= titanic.exploration.data$Age))
ggplot.relation.object <-ggplot.relation.object+geom_boxplot()+ggtitle("Survival vs Age Box Plot ")
ggsave(filename ="plots/titanic/Age_Vs_Survival.pdf" , plot=ggplot.relation.object)
ggplot.relation.object


#Create an Adult vs Child Survival table
titanic.exploration.data <- generate.new.feature.is.child(titanic.data = titanic.exploration.data)
prop.table(table(titanic.exploration.data$is.child,titanic.exploration.data$Survived),1)
table(titanic.exploration.data$is.child,titanic.exploration.data$Survived)

#create new variable family size
titanic.exploration.data$family.size <- generate.new.feature.family.size(titanic.data = titanic.exploration.data)
# Family Size vs Survived bar chart
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$family.size, fill = titanic.exploration.data$Survived))
ggplot.relation.object <-ggplot.relation.object+geom_bar()+ggtitle("Survived Bar Chart")
ggsave(filename ="plots/titanic/survival_vs_familysize.pdf" , plot=ggplot.relation.object)
ggplot.relation.object

# Fare distribution Histogram 
summary(titanic.exploration.data$Fare)
ggplot.relation.object <- ggplot(titanic.exploration.data, aes(x=titanic.exploration.data$Fare))
ggplot.relation.object <-ggplot.relation.object+geom_bar(colour="darkred", fill="white", binwidth =10)+ggtitle("Fare Histogram Chart")
ggsave(filename ="plots/titanic/Fare_Distribution.pdf" , plot=ggplot.relation.object)
ggplot.relation.object

#create new binned fare
titanic.exploration.data <- generate.new.feature.binned.fare(titanic.data = titanic.exploration.data)
prop.table(table(titanic.exploration.data$binned.fare,titanic.exploration.data$Survived),1)
table(titanic.exploration.data$binned.fare,titanic.exploration.data$Survived)

#Create correlogram that depicts correlation between variables. All varibles need to be numeric
corrgram.data <- titanic.exploration.data
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Sex <- as.numeric(corrgram.data$Sex)
#corrgram.data$Embarked <- revalue(corrgram.data$Embarked, c("C"=1,"Q"=2,"S"=3))
#corrgram.data$Embarked <- as.numeric(corrgram.data$Embarked)
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age","SibSp", "Parch", "Fare", "family.size")



corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=NuLL, 
         text.panel=panel.txt, main="Titanic Training Data", legend=TRUE)

#The positive correlations are shown in blue, while the negative correlations are shown in red. The darker the hue, the greater the magnitude of the correlation.
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.shade, upper.panel=NULL, 
         text.panel=panel.txt, main="Titanic Training Data")

#The positive correlations are shown in blue, while the negative correlations are shown in red. The darker the hue, the greater the magnitude of the correlation.
# E.g A completely postive correlated relation will be blue circle
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.shade, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data", legend=TRUE)