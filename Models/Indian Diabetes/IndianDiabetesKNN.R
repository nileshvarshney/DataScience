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
#   Transform & Split data into training set and test data set
#------------------------------------------------------------------------------------------#
set.seed(2017)
# Normalize method = "range"
# Data values can be scaled into the range of [0, 1] which is called normalization.
preProcess.Normalize <- preProcess(diabetes[,1:8],method = c("range") )
diabetes.normalize <- predict(preProcess.Normalize,diabetes[,1:8])

diabetes.normalize$Outcome <- as.factor(diabetes$Outcome)

trainIndex <- createDataPartition(diabetes.normalize$Outcome, p = .8, 
                                  list = FALSE, 
                                  times = 1)

diabetes.train.normalized <- diabetes.normalize[trainIndex,]
diabetes.test.normalized <- diabetes.normalize[-trainIndex,]

prop.table(table(diabetes.train.normalized$Outcome))
prop.table(table(diabetes.test.normalized$Outcome))

#------------------------------------------------------------------------------------------#
#  Fit KNN classificatio on training set 
#------------------------------------------------------------------------------------------#
# K-NN model Approach with Normalized data

# trControl Parameters settings
ctl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
) 

# Performance Parameters Setting
grid <- expand.grid(k = c(12,13,14,15,16,17,18,19))
model.knn <- train(
  Outcome ~ ., 
  data = diabetes.train.normalized, 
  method = "knn",
  tuneGrid = grid,
  trControl = ctl)

# The final value used for the model was k = 19.

# Plot Model
plot(model.knn)
#------------------------------------------------------------------------------------------#
#  Predict the test data on built model 
#------------------------------------------------------------------------------------------#

# Predict test set data value
predict.knn <- predict(model.knn,diabetes.test.normalized)

# Check the model performance through confuse matrix
gmodels::CrossTable(predict.knn,diabetes.test.normalized$Outcome)


#------------------------------------------------------------------------------------------#
# Through Class package by using above K value
#------------------------------------------------------------------------------------------#
library(class)

model.knn.normalize <- knn(diabetes.train.normalized[,1:8],
                           diabetes.test.normalized[,1:8],
                           cl = diabetes.train.normalized[,9],
                           k = 19 )

crosstab <- gmodels::CrossTable(model.knn.normalize,diabetes.test.normalized[,9])

# y
# x    0  1
# 0   88 23
# 1   12 30
round(sum(diag(crosstab$t)) *100/nrow(diabetes.test.normalized),2)

# Model performance 77.12%




