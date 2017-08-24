#-------------------------------------------------------------------------------------------#
###  Purpose : Estimate the quality of the wines through regression Tree and Models Tree    #
#-------------------------------------------------------------------------------------------#
#---------------------------------------------------
# Set Working directory and imclude package
#--------------------------------------------------
setwd("~/Google Drive/Gits/DataScience/Models/RegressionTree")
library(ggplot2)
library(rpart)
library(rpart.plot)
#---------------------------------------------------
# Import white wine data
#---------------------------------------------------
wine <- read.csv("whitewines.csv")

#---------------------------------------------------
# Exploring and preparing Data
#---------------------------------------------------
ggplot(data = wine) +
  geom_histogram(aes(x = quality)) 

wine.sample <- sample(nrow(wine), (round(nrow(wine) *0.8)))
wine.train <- wine[wine.sample,]
wine.test <- wine[-wine.sample,]

#---------------------------------------------------
# Training a model on training set of data
#---------------------------------------------------
moded.rpart.1 <- rpart(quality ~ . , data = wine.train)  
# Since quality is continous variable, it will use anova method
rpart.plot(moded.rpart.1,type = 3,fallen.leaves = TRUE,digits = 3,extra = 101)

#---------------------------------------------------
# Evaluate Model Peroformance
#---------------------------------------------------
p.rpart.1 <- predict(moded.rpart.1,wine.test)

summary(p.rpart.1)
summary(wine.test$quality)

# Model is not correctly identifying extreame cases,

cor(p.rpart.1,wine.test$quality)  ## 0.5099542
# corelation 0.50 is acceptable as it measures how strongly prediction is related to actual. But it does not measure how far it is from actual.

# Measure performance through Mean Absolute Error
MAE <- function(actual,predicted){
  mean(abs(actual - predicted))
}

MAE(wine.test$quality,p.rpart.1) # 0.6002462 ( Prediction)
MAE(wine.train$quality,predict(moded.rpart.1,wine.train)) # 0.5916761 ( Training set)
# The mean absolute error between actual and predicted is 0.60 which is very close to MAE for training set. it soemehat indicates that classification is very close. Lets check the MAE between mean of training set and training set

mean(wine.train$quality)  # 5.88489
MAE(5.88489, wine.test$quality) # 0.6616

# MAE between actual training set and test set is 0.6616, which is not so close to 0.60. Hence  Model can still be improved.

#----------------------------------------------------------
# Lets try  M5P function from Rweka
#----------------------------------------------------------
#library(RWeka)  # Not extending it as error in installing RWeka on Mac

# Since there is change for improvement, I am going to give try to SVM algorithm

#----------------------------------------------------------
# SVM ( Support Vector Machine)
#----------------------------------------------------------

