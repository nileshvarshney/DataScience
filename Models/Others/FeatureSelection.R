#-------------------------------------------------------------
# Feature Selection with the Caret R Package
#-------------------------------------------------------------
#  Remove Redundant Features (reduce pair-wise correlations)
#-------------------------------------------------------------
# Data can contain attributes that are highly correlated with each other. Many methods perform better if highly correlated attributes are removed.G enerally, you want to remove attributes with an absolute correlation of 0.75 or higher.
set.seed(7)
library(mlbench)
library(caret)
# Load data
diabeties <-PimaIndiansDiabetes
# Calculate correlation Matrix
correlationMatrix <- cor(diabeties[,1:8])
#print correlation Matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff = 0.5)
print(highlyCorrelated)
# This function searches through a correlation matrix and returns a vector of integers corresponding to columns to remove to reduce pair-wise correlations. In our case it is returning 8, it mean 8th variable need to be removed.

# Example -  mtcars
correlationMatrix.mtcars = cor(mtcars)
print(correlationMatrix.mtcars)
highlyCorrelated.mtcars <- findCorrelation(correlationMatrix.mtcars,cutoff = 0.75)
print(highlyCorrelated.mtcars)


# Example -  iris
correlationMatrix.iris = cor(iris[,1:4])
print(correlationMatrix.iris)
highlyCorrelated.iris <- findCorrelation(correlationMatrix.iris,cutoff = 0.75)
print(highlyCorrelated.iris)
#-------------------------------------------------------------
# Rank Features By Importance
#-------------------------------------------------------------
# prepare training scheme
ctrl <- trainControl(method = "repeatedcv",number = 10,  repeats = 3)
#train the model
model <- train(diabetes ~ .,
               data = diabeties, 
               method = "lvq",
               preProcess = "scale",
               trControl = ctrl)
# estimate variable importance
importance <- varImp(model,scale = FALSE)

# summarize importance
print(importance)
plot(importance)

# Example mtcars
ctrl.mtcars <- trainControl(method = "repeatedcv",number = 10,  repeats = 3)
model.mtcars <- train(mpg ~ .,
               data = mtcars, 
               method = "lm",
               trControl = ctrl.mtcars)

importance.mtcars <- varImp(model.mtcars,scale = FALSE)
print(importance.mtcars)

plot(importance.mtcars)

# Example iris
ctrl.iris <- trainControl(method = "repeatedcv",number = 10,  repeats = 3)
model.iris <- train(Species ~ .,
                      data = iris, 
                      method = "C5.0",
                      trControl = ctrl.iris)

importance.iris <- varImp(model.iris,scale = FALSE)
print(importance.iris)

plot(importance.iris)
#-------------------------------------------------------------
# Feature Selection
#-------------------------------------------------------------
# Automatic feature selection methods can be used to build many models with different subsets of a dataset and identify those attributes that are and are not required to build an accurate model.

# A popular automatic method for feature selection provided by the caret R package is called Recurcive Feature Elimination or RFE

# define the control using a random forest selection function
ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
results <- rfe(diabeties[,1:8], 
               diabeties[,9], 
               sizes=c(1:8), 
               rfeControl=ctrl
               )

# summarize the results
print(results)

# list the chosen features
predictors(results)

# plot the results
plot(results, type=c("o","g"))


# Example mtcars
ctrl.mtcars <- rfeControl(functions=lmFuncs, method="cv", number=10)
results.mtcars <- rfe(mtcars[,2:11], 
               mtcars[,1], 
               sizes=c(2:11), 
               rfeControl=ctrl.mtcars
)
# summarize the results
print(results.mtcars)

# list the chosen features
predictors(results.mtcars)

# plot the results
plot(results.mtcars, type=c("o","g"))









