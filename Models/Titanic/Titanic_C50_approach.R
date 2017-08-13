setwd("~/Google Drive/Gits/DataScience/Models/Titanic")
# Import data set 
titanic.train <- read.csv("train.csv")
titanic.test <- read.csv("test.csv")

library(dplyr)
test <- titanic.test
test$Survived <- NA
test1 <- test %>% select(PassengerId,Survived,Pclass:Embarked)
titanic.full <- rbind(titanic.train,test1)
rm(test1)
rm(test)

#-----------------------------------------------------------------------------
### Explore and prepare data for analysis
#-----------------------------------------------------------------------------
summary(titanic.full)

# Replace NA with mean value for fare
titanic.full$Fare <- ifelse(is.na(titanic.full$Fare),
                            mean(titanic.full$Fare,na.rm = TRUE),
                            titanic.full$Fare)

#Replacing Age Missing value with the median Age
titanic.full$Age <- ifelse(
  is.na(titanic.full$Age), 
  median(titanic.full$Age,na.rm = TRUE),
  titanic.full$Age
)

# Adding Family Indicator and Cabin Type
titanic.full$SibSp <- ifelse(titanic.full$SibSp > 0, 1,0)
titanic.full$Parch <- ifelse(titanic.full$Parch > 0, 1,0)
titanic.full$Family <- ifelse((titanic.full$Parch + titanic.full$SibSp) > 0,1,0)
titanic.full <- subset(titanic.full,select = -c(SibSp,Parch,Name))
titanic.full$Cabin <-substr(titanic.full$Cabin,1,1)
titanic.full$Cabin <-ifelse(titanic.full$Cabin =='',"X",titanic.full$Cabin)  # No Cabin (X)
titanic.full$Embarked <-as.factor(ifelse(as.character(titanic.full$Embarked) =="","S",as.character(titanic.full$Embarked)))

# Move data back to training and test set
titanic.train.clean <- titanic.full[1:891,]
titanic.test.clean <- titanic.full[892:1309,]
titanic.test.clean <- titanic.test.clean[,!(names(titanic.test.clean) %in% c("Survived"))]

#-----------------------------------------------------------------------------
####Exploratory Data Analysis and Plots Servived passanger proportion
#-----------------------------------------------------------------------------

round(prop.table(table(titanic.train.clean$Survived)),2) # 38% servived
#Gender wise passanger proportion

round(prop.table(table(titanic.train.clean$Sex)),2)  # 35/65
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Sex)),2)
library(ggplot2)
ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Sex)
# Female of all age was able to survived better, male did not made it.

# PClass wise passanger proportion
round(prop.table(table(titanic.train.clean$Pclass)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Pclass)),2)

ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Pclass)
# Plot indicates PClass -1 pasanger survival was better among other PClasses. Survival chance in Pclass 2 was 50%. There was only 13% passengers survivived in PClass 3 even through this class covering the 55% passengers.

# Cabin wise passanger proportion
round(prop.table(table(titanic.train.clean$Cabin)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Cabin)),2)
# Survival percent of passangers in cabin B,C,D,E is better compared to A and F. Cabin X which is default cabin affected most.

# Embarked wise passanger proportion
round(prop.table(table(titanic.train.clean$Embarked)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Embarked)),2)

ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Embarked)
# Passangers from Embarked S is affected most.

# Family wise passanger proportion
round(prop.table(table(titanic.train.clean$Family)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Family)),2)

ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Family)

# Passanger who was travelling in family was survived better.

# Divide data in training set and test set (730/161)

# Purpose to divide the training dataset to measure the efficiency of the model

set.seed(2017)
library(rpart)
train.sample <- sample(891,730)
train.set <- titanic.train.clean[train.sample,]
test.set <- titanic.train.clean[-train.sample,]

#---------------------------------------------------------------
### Use C5.0  Decision Trees and rules Based Model
#---------------------------------------------------------------
library(C50)
model.c50 <- C5.0( 
  x = train.set[-c(1,2,6)], y = as.factor(train.set$Survived) 
  , trials = 3)

pred_model <- predict(
  model.c50,
  test.set[-c(1,2,6)]
  )

library(gmodels)
CrossTable(test.set$Survived, pred_model,prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE,
           dnn =c ("Actual Survived", "Predicted Durvived"))



pred_final <- pred_model <- predict(
  model.c50,
  titanic.test.clean[-c(1,5)]
)

result <- data.frame(PassengerId = titanic.test.clean$PassengerId, Survived = pred_final)

write.csv(result,"result.csv")


