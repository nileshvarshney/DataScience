# ------------------------------------------------------------------------------------- #
#  Data Set : Pima Indian Diabetes                                                      #
#  This dataset is originally from the National Institute of Diabetes and Digestive and #
#  Kidney Diseases. The objective is to predict based on diagnostic measurements        #
#  whether a patient has diabetes.                                                      #
# ------------------------------------------------------------------------------------- #


#------------------------------------------------------------------------------------------#
#  Read Provided sample data
#------------------------------------------------------------------------------------------#
diabetes <- read.csv("diabetes.csv")
str(diabetes)

#------------------------------------------------------------------------------------------#
#  Exploratory Data analysis
#------------------------------------------------------------------------------------------#
library(ggplot2)
library(psych)
library(dplyr)
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

# Lets get model performance before multicollinearity
model.pre <- glm( Outcome ~ . , 
                  data = diabetes,
                  binomial(link = "logit"))

summary(model.pre)
# BloodPressure, SkinThickness and age are not sttistically significant.
# Glucose, BMI ,DiabetesPedigreeFunction and Pregnancies are statitstically significant

#### Detecting mullticollenearity   
library(psych)
diabetes.predictor <- data.frame(scale(diabetes[,1:8]))
pairs.panels(scale(diabetes.predictor))


# Calculate the Correlation  matrix
diabetes.predictor.cor <- cor(diabetes.predictor)
diabetes.predictor.cor

# There is some chance of mullticollenearity between BMI and SkinThickness

# Eigen System Analysis
eigen(diabetes.predictor.cor)$value

# Ratio of max and min eigen value 
max(eigen(diabetes.predictor.cor)$value)/min(eigen(diabetes.predictor.cor)$value)

# Ratio is just 5.611594. Chance of mullticollenearity is quite low.

kappa(diabetes.predictor.cor,exact = TRUE)  # 5.611594

#### Detecting mullticollenearity  ( Through car package)
library(car)

vif(model.pre) # None are around 5, hence there is no mullticollenearity exists in the data.

mean(vif(model.pre)) # 1.323603 this shows there is some mullticollenearity exists.

# Mostof the mullticollenearity does not confirms hence, we rejects the mullticollenearity.

#------------------------------------------------------------------------------------------#
#  Split data into training set and test data set
#------------------------------------------------------------------------------------------#
set.seed(2017)
trainIndex <- createDataPartition(diabetes$Outcome, p = .8, 
                                  list = FALSE, 
                                  times = 1)

diabetes.train <- diabetes[trainIndex,]
diabetes.test <- diabetes[-trainIndex,]

prop.table(table(diabetes.train$Outcome))
prop.table(table(diabetes.test$Outcome))

#------------------------------------------------------------------------------------------#
#  Fit Logistic regression on training set 
#------------------------------------------------------------------------------------------#
model.logit <- glm(Outcome ~ . , 
                   data = diabetes.train,
                   binomial(link = "logit"))

summary(model.logit)

reduced.model.logit  <- step(model.logit,direction = "backward")

summary(reduced.model.logit)
plot(reduced.model.logit)

coef(summary(reduced.model.logit))

# Compare models through anova
anova(reduced.model.logit, model.logit)

# check for multicollinearity in the model
vif(reduced.model.logit)  # All variables are below 4.

library(InformationValue)

#------------------------------------------------------------------------------------------#
#  Predict the test data on built model 
#------------------------------------------------------------------------------------------#
reduced.predict.logit <- predict(reduced.model.logit, newdata = diabetes.test,type = "response")

# Gettting result in predicted probability
predicted.logit <- plogis(predict(reduced.model.logit, diabetes.test, type="response"))

as.data.frame(predicted.logit)

InformationValue::optimalCutoff(diabetes.test$Outcome,predicted.logit)[1]
# Cut of value  0.6434325

# Misclassification Error
misClassError(diabetes.test$Outcome, predicted.logit, threshold = 0.6434325) # 0.2222
# lower the misclassification error, the better is  model.

# Receiver Operating Characteristics Curve to trace the percentage of true positives accurately predicted
plotROC(diabetes.test$Outcome, predicted.logit)
# for a good model, the curve should rise steeply, indicating that the TPR (Y-Axis) increases faster than the FPR (X-Axis) as the cutoff score decreases. Greater the area under the ROC curve, better the predictive ability of the model.


# Concordance
Concordance(diabetes.test$Outcome, predicted.logit) # 0.846213
# For a perfect model, this will be 100%. So, the higher the concordance, the better is the quality of model.

# Sensitivity(or True Positive Rate)
sensitivity(diabetes.test$Outcome, predicted.logit, threshold = 0.6434325) # 0.4509804

# specificity (percentage of 0’s (actuals) correctly predicted.)
specificity(diabetes.test$Outcome, predicted.logit, threshold = 0.6434325) #  0.9411765

# The above numbers are calculated on the validation sample that was not used for training the model. So, a truth detection rate of 45.09% on test data is good.

# Confusion Matrix
confusionMatrix(diabetes.test$Outcome, predicted.logit, threshold = 0.6434325)

# 0  1
# 0 96 28
# 1  6 23









