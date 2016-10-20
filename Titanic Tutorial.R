setwd("C:/Users/hdanielsson/OneDrive - Kal Tire/Data Science/Kaggle/Titanic")
#Import files into data frames
train <- read.csv("C:/Users/hdanielsson/OneDrive - Kal Tire/Data Science/Kaggle/Titanic/train.csv")
#View(train)
test <- read.csv("C:/Users/hdanielsson/OneDrive - Kal Tire/Data Science/Kaggle/Titanic/test.csv")
#View(test)

#######################################################################################
#Investigations
#Looking at the STRucture of the data frame
str(train)
#looking at a specific variable
train$Survived

#sum up the values for a particular variable
table(train$Survived)
#and then format the result into a percentage
prop.table(table(train$Survived))

#We first looked at the survived column only and assumed everyone died
#We then looked at the Sex column and assume all men died
#We'll look at survival and sex in two dimensions
prop.table(table(train$Sex, train$Survived),1)

# Third, lets look at the age variable
# we see that 177 are missing values, we'll assume these are median age, i.e adults
#Summarize a variable
summary(train$Age)
#we'll add a Child column to indicate who's under 18
train$Child <-0
train$Child[train$Age < 18] <- 1
#then we'll look at how many survived of each group. still looks like most females survive regardless of age group
aggregate(Survived ~ Child + Sex, data=train, FUN = function(x) {sum(x)/length(x)})
#let's look at the class and what they paid
#the class looks nice with 3 values but the fare is numberical so we group them into 4 groups
#and then agreggate all the variables
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN = function(x) {sum(x)/length(x)})
##########################################################################################
#Predictions

#scenario 1
#we will predict that no one survived so we'll add a new column survived with a zero for each of the 418 rows (REPeate 418 times)
#or we can simple assign 0 to all rows with test$survived <- 0
test$Survived <- rep(0,418)

#Scenario 2
#We will predict that only women survived
test$Survived[test$Sex == 'female'] <- 1

#Scenario 3
#women in class 3 that paid more than $20 don't seem to survive
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#We'll then add this to a new data frame called submit and write it to a new CSV file
submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allmendieandclass3womenwith expensivetickets.csv", row.names = FALSE)