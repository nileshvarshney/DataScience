#-----------------------------------------------------------------
# Diagnosing Breast Cancer through Classification
#-----------------------------------------------------------------
#--------------------------------------------------
# Include required Library and set working directory
#--------------------------------------------------
setwd("~/Google Drive/Gits/DataScience/Models/DiagnosingBreastCancer")
library(dplyr)
library(caret)
set.seed(3456)

#--------------------------------------------------
# Read Sample data
#--------------------------------------------------
cancer <- read.csv("data.csv")
cancer <- cancer %>%
  select(-c(X,id))

#--------------------------------------------------
# Exploratory Data Analysis & Transform
#--------------------------------------------------
# Normalize method = "range"
# Data values can be scaled into the range of [0, 1] which is called normalization.
preProcess.Normalize <- preProcess(cancer[,2:31],method = "range" )
cancer.normalize <- predict(preProcess.Normalize,cancer[,2:31])
cancer.normalize$diagnosis <- cancer$diagnosis

# standardize method=c("center", "scale")
# The scale transform calculates the standard deviation for an attribute and divides each value by that standard deviation.
# Combining the scale and center transforms will standardize your data. Attributes will have a mean value of 0 and a standard deviation of 1.

# Center method=c("center")
# The center transform calculates the mean for an attribute and subtracts it from each value.

# Box-Cox Transform method=c("BoxCox")
# When an attribute has a Gaussian-like distribution but is shifted, this is called a skew. The distribution of an attribute can be shifted to reduce the skew and make it more Gaussian. The BoxCox transform can perform this operation (assumes all values are positive).

# Principal Component Analysis  method=c("center", "scale", "pca"))
# Transform the data to the principal components. The transform keeps components above the variance threshold (default=0.95) or the number of components can be specified (pcaComp). The result is attributes that are uncorrelated, useful for algorithms like linear and generalized linear regression.

# Independent Component Analysis method=c("center", "scale", "ica")
# Transform the data to the independent components. Unlike PCA, ICA retains those components that are independent. You must specify the number of desired independent components with the n.comp argument. Useful for algorithms such as naive bayes.



#--------------------------------------------------
# Divide data in training set and test set
#--------------------------------------------------
# Without transformation
trainIndex <- createDataPartition(cancer$diagnosis, p = .8, 
                                  list = FALSE, 
                                  times = 1)
cancer.train <- cancer[trainIndex,]
cancer.test <- cancer[-trainIndex,]

# Normalize data
cancer.normalize.train <- cancer.normalize[trainIndex,]
cancer.normalize.test <- cancer.normalize[-trainIndex,]

#--------------------------------------------------
# Create Model and Tune it
#--------------------------------------------------
# K-NN model Approach with Normalized data

# trControl Parameters settings
ctl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
) 

# Performance Parameters Setting
grid <- expand.grid(k = c(12,13,14,15,16))


# Fit model in training set of data
set.seed(2345)
model.knn <- train(
  diagnosis ~ ., 
  data = cancer.normalize.train,
  method = "knn" ,
  tuneGrid = grid,
  trControl = ctl)

# Plot Model
plot(model.knn)
# The final value used for the model was k = 15.

# Predict test set data value
predict.knn <- predict(model.knn,cancer.normalize.test)

# Check the model performance through confuse matrix
confusionMatrix(predict.knn, cancer.normalize.test$diagnosis) # Accuracy : 0.9912


# Throuh class package
library(class)
model.knn.normalize <- knn(cancer.normalize.train[,1:30],
                           cancer.normalize.test[,1:30],
                           cl = cancer.normalize.train[,31],
                           k = 15 )

library(gmodels)
CrossTable(model.knn.normalize,cancer.normalize.test[,31])
#--------------------------------------------------
# Make prediction and Conclusion
#--------------------------------------------------
# Model Efficiency is 99.12%. It is quite book, model is able to predict all the case.