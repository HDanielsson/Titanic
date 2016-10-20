setwd("C:/Users/hdanielsson/OneDrive - Kal Tire/Data Science/Kaggle/Titanic")
#Import files into data frames
train <- read.csv("C:/Users/hdanielsson/OneDrive - Kal Tire/Data Science/Kaggle/Titanic/train.csv")
#View(train)
test <- read.csv("C:/Users/hdanielsson/OneDrive - Kal Tire/Data Science/Kaggle/Titanic/test.csv")
#View(test)

#Load libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#library(Amelia)

#build decision tree and put results in fit and then put the prediction into Prediction
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")

#open an interactive window allowing me to kill nodes I don't like
fit2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data=train,
              method="class",
              control = rpart.control(minsplit=2, cp=0))
new.fit <- prp(fit2,snip=TRUE)$obj
fancyRpartPlot(new.fit)
MyPrediction <- predict(new.fit, test, type = "class")

#We'll then add this to a new data frame called submit and write it to a new CSV file
submit <- data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstdecisiontree.csv", row.names = FALSE)

#submitting my manually tweaked tree
mysubmit <- data.frame(PassengerID = test$PassengerId, Survived = MyPrediction)
write.csv(mysubmit, file = "manualdecisiontree.csv", row.names = FALSE)
