# ------------------------------------------------------------------------------------- #
#  Data Set : Pima Indian Diabetes                                                      #
#  This dataset is originally from the National Institute of Diabetes and Digestive and #
#  Kidney Diseases. The objective is to predict based on diagnostic measurements        #
#  whether a patient has diabetes.                                                      #
# ------------------------------------------------------------------------------------- #

#### Read Provided sample data
diabetes <- read.csv("diabetes.csv")
str(diabetes)

#### Exploratory Data analysis
library(ggplot2)
library(psych)
library(dplyr)
library(caret)
pairs.panels(diabetes)

# Glucose,BMI age and pregnanies are highly highly and moderately correlated with the outcomes.
# There is possibility of multi-collinearity between skinthickness, insulin, glucose, age 

## Pregnancies data
table(diabetes$Pregnancies)

## Glucose Data
table(diabetes$Glucose)

## Blood Pressure
table(diabetes$BloodPressure) # There are 35 peoples where BP is 0. That is not technically possiable
summary(diabetes$BloodPressure) # Median blood pressure is 72. 

# Replace 0 blood pressure with median blood pressuure
diabetes$BloodPressure <- ifelse(diabetes$BloodPressure == 0, 
                                 median(diabetes$BloodPressure,na.rm = TRUE),
                                 diabetes$BloodPressure
                                 )


## Skin Thickness
table(diabetes$SkinThickness)  #triceps skin-fold thickness normal value for female 23. This value can not be 0
summary(diabetes$SkinThickness)
# Lets replace this value median value which is also 23.

diabetes$SkinThickness <- ifelse(
  diabetes$SkinThickness == 0 , 
  median(diabetes$SkinThickness,na.rm = TRUE),
  diabetes$SkinThickness)

ggplot(data = diabetes) +
  geom_boxplot(aes(x =  as.factor(Outcome), y = SkinThickness),
               outlier.colour = "red", outlier.size = 2.5)

# There are lot of outlier, but skin thickness can be 99 if a person is fatty irrespective to wether she is diabetics or not. Box plot is also shows diabetics has higher SkinThickness.

## BMI
table(diabetes$BMI == 0) # There are 11 observation where 0 BMI information provided. It is practically not possible.
summary(diabetes$BMI)

filter(diabetes, BMI == 0) # There are 9 out of 11 are not diabetics, Lets replace this BMI with healthy BMI at higher end to 30

diabetes$BMI <- ifelse(diabetes$BMI == 0, 32, diabetes$BMI)
ggplot(data = diabetes) +
  geom_boxplot(aes(x =  as.factor(Outcome), y = BMI),
               outlier.colour = "red", outlier.size = 2.5)

# There are few outliers as per boxplot. But outliers range looks quite ok. Box plot is also shows diabetics has higher BMI.

## DiabetesPedigreeFunction
ggplot(data = diabetes) +
  geom_boxplot(aes(x =  as.factor(Outcome), y = DiabetesPedigreeFunction),
               outlier.colour = "red", outlier.size = 2.5)

# Age
ggplot(data = diabetes) +
  geom_boxplot(aes(x =  as.factor(Outcome), y = Age),
               outlier.colour = "red", outlier.size = 2.5)

# Older people has more chance to be diabetics

prop.table(table(diabetes$Outcome)) # data set 35% diabetics

## Correlation 
pairs.panels(diabetes)

# Glucose,BMI age and pregnanies are highly and moderately correlated with the outcomes.
# There is possibility of multi-collinearity between Age abd Blood Pressure. Insulin and Glucose, Skin Thickness and BMI

#------------------------------------------------------------------------------------------#
#  Split data into training set and test data set
#------------------------------------------------------------------------------------------#
set.seed(2017)

trainIndex <- createDataPartition(diabetes$Outcome, p = .8, 
                                  list = FALSE, 
                                  times = 1)


diabetes$Outcome <- as.factor(diabetes$Outcome)

diabetes.train <- diabetes[trainIndex,]
diabetes.test <- diabetes[-trainIndex,]

prop.table(table(diabetes.train$Outcome))
prop.table(table(diabetes.test$Outcome))

#------------------------------------------------------------------------------------------#
#  Fit C50 in training set data
#------------------------------------------------------------------------------------------#
# C50 model Approach with Normalized data

# trControl Parameters settings
ctl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
) 

# Performance Parameters Setting
grid <- expand.grid(trials = c(1,5,10,15,20,25), winnow = FALSE,model = "tree" )
model.c50 <- train(
  Outcome ~ ., 
  data = diabetes.train, 
  method = "C5.0",
  tuneGrid = grid,
  trControl = ctl)

# The final values used for the model were trials = 15, model = tree and winnow = FALSE.

# Plot Model
plot(model.c50)
#------------------------------------------------------------------------------------------#
#  Predict the test data on built model 
#------------------------------------------------------------------------------------------#

# Predict test set data value
predict.c50 <- predict(model.c50,diabetes.test)

# Check the model performance through confuse matrix
gmodels::CrossTable(predict.c50,diabetes.test$Outcome)

confusionMatrix(predict.c50,diabetes.test$Outcome) # Accuracy : 0.7386   

#------------------------------------------------------------------------------------------#
# Through C50 package by using above tunning parameters
#------------------------------------------------------------------------------------------#
library(C50)

c50.ctl <- C5.0Control(winnow = FALSE)
model.c50.2 <- C5.0(Outcome ~ ., 
                    data = diabetes.train,
                    rules = FALSE,
                    trials = 15,
                    control = c50.ctl)



predict.c50.2 <- predict(model.c50.2,newdata = diabetes.test,type = "class" )

cmatric <- confusionMatrix(predict.c50.2,diabetes.test$Outcome)

# Model Accuracy : 0.7386  
# Sensitivity : 0.7941          
# Specificity : 0.6275   
# 
# Reference
# Prediction  0  1
# 0 81 19
# 1 21 32


