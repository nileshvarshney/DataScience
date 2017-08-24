# -----------------------------------------------------------
# Logistic Regression Approach on Titanic data
# -----------------------------------------------------------
# Inlcude required Library
library(caret)
library(dummies)
library(psych)


# Set Working directory and import dataset
setwd("~/Google Drive/Gits/DataScience/Models/Titanic")
titanic <- read.csv("train.csv")


# Exploratory data analysis
str(titanic)
# Remove Unnecessary Variables
titanic <- subset(titanic,select = -PassengerId)  # removed passanger Ids
titanic <- subset(titanic, select = -Name) # Removed Name
titanic <- subset(titanic, select = -Ticket) # Removed Ticket

# Replace All Missing Age with mean value of age
mean_age <- round(mean(titanic$Age,na.rm = TRUE))
titanic$Age <- ifelse(is.na(titanic$Age),mean_age,titanic$Age)

# Merge SibSp and Parch with family indicator
titanic$SibSp <- ifelse(titanic$SibSp > 0, 1,0)
titanic$Parch <- ifelse(titanic$Parch > 0, 1,0)
titanic$Family <- titanic$SibSp + titanic$Parch
titanic$Family <- ifelse(titanic$Family > 0,1,0)
titanic <- subset(titanic, select = -SibSp) 
titanic <- subset(titanic, select = -Parch) 

# Clean data for Cabin by  taking first char of the Cabin to Indetify cabin class
titanic$Cabin <-   substr(titanic$Cabin,1,1) # 687 passange with no cabin information
# Assign dummy cabin 'x' for whom cabin data is not present
titanic$Cabin <- ifelse(titanic$Cabin =='','X',titanic$Cabin)

# There are couple of passange have no Embarked information. Let consider than 'S' as most of the passage started from this Embarked
titanic$Embarked <-as.factor(ifelse(as.character(titanic$Embarked) =="","S",as.character(titanic$Embarked)))

# Add dummy variable for Pclass, Sex, Cabin and Embarked
# Pclass <- dummy(titanic$Pclass)
# Sex <- dummy(titanic$Sex)
# Cabin <- dummy(titanic$Cabin)
# Embarked <- dummy(titanic$Embarked)
# 
# titanic <- cbind(titanic,Pclass)
# titanic <- cbind(titanic,Sex)
# titanic <- cbind(titanic,Cabin)
# titanic <- cbind(titanic,Embarked)
# 
# titanic <- subset(titanic, select = -c(Pclass,Sex,Cabin,Embarked))

# -------------------------------------------------------------------------------
# Correlation Plot
# -------------------------------------------------------------------------------
pairs.panels(titanic)


# -------------------------------------------------------------------------------
# Logistic Model
# -------------------------------------------------------------------------------
model.logist <- glm(Survived ~ ., data = titanic,family = binomial("logit"))











