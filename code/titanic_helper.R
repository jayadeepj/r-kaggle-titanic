
#----------------------------------------- Functions ---------------------------
#Find the Title of the Passenger Mr, Miss
title.extraction.function <- function(name.original)
{
  
  name.split <- strsplit(name.original, ", ")[[1]][2]
  title <- strsplit(name.split, ". ")[[1]][1]
  return (title)
}


fetch.mean.age.for.title.function <- function(title,aggregate.age.per.title)
{
  return (aggregate.age.per.title$MeanAge[aggregate.age.per.title$Title==title])
}

generate.new.feature.is.child <- function(titanic.data)
{
  titanic.data$is.child<-'ADULT'
  titanic.data$is.child[titanic.data$Age<=12]<- 'CHILD'
  titanic.data$is.child <- as.factor(titanic.data$is.child)
  return(titanic.data)
}

generate.new.feature.binned.fare <- function(titanic.data)
{
  titanic.data$binned.fare <- '30+'
  titanic.data$binned.fare[titanic.data$Fare<30 & titanic.data$Fare>=20]  <- '20-30'
  titanic.data$binned.fare[titanic.data$Fare<20 & titanic.data$Fare>=10]  <- '10-20'
  titanic.data$binned.fare[titanic.data$Fare<10]  <- '<10'
  titanic.data$binned.fare <- as.factor(titanic.data$binned.fare)
  return(titanic.data)
}

generate.new.feature.family.size <- function(titanic.data)
{
  titanic.data$family.size <- titanic.data$Parch+titanic.data$SibSp
  return(titanic.data)
}

clean.features.one <- function(titanic.data)
{
  # Convert appropriate columns as factors
  titanic.data$Survived <- as.factor(titanic.data$Survived )
  titanic.data$Pclass <- as.factor(titanic.data$Pclass )
  titanic.data$Name<-as.character(x =titanic.data$Name)
  titanic.data$Embarked[which(is.na(titanic.data$Embarked))] <- 'S'
  titanic.data$Embarked <- as.factor(titanic.data$Embarked)
  return(titanic.data)
}

filter.features.one <- function(titanic.data)
{
  #Remove Unwanted Columns
  titanic.data$Ticket<-NULL
  titanic.data$Fare<-NULL
  titanic.data$Cabin<-NULL
  
  titanic.data$PassengerId<-NULL
  titanic.data$Embarked<-NULL
  
  return(titanic.data)
}