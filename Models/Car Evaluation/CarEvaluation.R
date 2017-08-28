#-------------------------------------------------------------------
# Car Evaluation - A classification problem
#-------------------------------------------------------------------
# There are 4 classification available in provided sample data()
# ---------------------------------------------------------
# Include required libraries
# ---------------------------------------------------------
library(caret)


# ---------------------------------------------------------
# Read full data set and add columns name
# ---------------------------------------------------------
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"
car <- read.table(url, sep = ",")
names(car) = c("buying","maint","doors","persons","lug_boot","safety","class")

# ---------------------------------------------------------
# Explratory Data Analysis
# ---------------------------------------------------------
str(car)
prop.table(table(car$buying))   # 4 caegory equally  distributed

prop.table(table(car$maint))    # 4 caegory equally  distributed

prop.table(table(car$doors))    # 4 caegory equally  distributed

prop.table(table(car$persons))  # 3 caegory equally  distributed

prop.table(table(car$lug_boot)) # 3 caegory equally  distributed

prop.table(table(car$safety))   # 3 caegory equally  distributed

prop.table(table(car$class))    # 4 category with different distribution

# ---------------------------------------------------------
# Divide data in training and test dataset
# ---------------------------------------------------------
set.seed(1234)
trainIndex <- createDataPartition(car$class, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train.car <- car[trainIndex,]
test.car <- car[-trainIndex,]

# ---------------------------------------------------------
# Train the model on train set data - C5.0
# ---------------------------------------------------------
getModelInfo("C5.0")

grid <- expand.grid(
  trials = c(40,41,42,43,44,45),
  model = c("rules"),
  winnow = FALSE
)

fitControl <- trainControl(method = "repeatedcv",
               number = 10,
               repeats = 10,
               returnResamp="all")

model.c50 <- train(class ~ ., 
                   data = train.car, 
                   method = "C5.0",
                   tuneGrid = grid,
                   trControl = fitControl)

xyplot(model.c50,type = c("g","p","smooth"))
# ---------------------------------------------------------
# Train the model on train set data - C5.0
# ---------------------------------------------------------
predict.c50 <- predict(model.c50,test.car)

confusionMatrix(test.car$class,predict.c50)  # Accuracy : 0.9942 

#----------------------------------------------------------------------------------
# RPART Implementation
#----------------------------------------------------------------------------------
grid <- expand.grid(
  cp = c(0.01,0.02,0.03,0.04,0.050,06,0.07,0.08,0.09))

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           returnResamp="all")
model.rpart <- train(class ~ ., 
                     data = train.car,
                     method = "rpart",
                     tuneGrid = grid,
                     trControl = fitControl)
# The final value used for the model was cp = 0.01.
plot(model.rpart)
predict.rpart <- predict(model.rpart,test.car)

confusionMatrix(test.car$class,predict.rpart) # Accuracy : 0.8663   

#----------------------------------------------------------------------------------
# SVM  Linear Kernel Implementation
#----------------------------------------------------------------------------------
# SVL Linear (Speed, Not explainable, and data set is also not big)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           returnResamp="all")

grid <- expand.grid(
  C = c(1,2,3,4,5,6,7,8))


model.svm.linear <- train(class ~ ., 
                   data = train.car,
                   method = "svmLinear",
                   tuneGrid = grid,
                   trControl = fitControl)
# Accuracy was used to select the optimal model using  the largest value. The final value used for the model was C = 6.
plot(model.svm.linear)

predict.svm.linear <- predict(model.svm.linear,test.car)

confusionMatrix(test.car$class,predict.svm.linear) # Accuracy : 0.9506  

#----------------------------------------------------------------------------------
# SVM  Radial Kernel Implementation ( Kernel Implemetation)
# ((slow but more accurate)
#----------------------------------------------------------------------------------
grid <- expand.grid(
  C = c(7,8,9,10,11), sigma = c(0.05,0.055,0.06,0.07))


model.svm.radial <- train(class ~ ., 
                          data = train.car,
                          method = "svmRadial",
                          tuneGrid = grid,
                          trControl = fitControl)

# The final values used for the model were sigma = 0.05 and C = 11.
plot(model.svm.radial)
predict.svm.radial <- predict(model.svm.radial,test.car)

confusionMatrix(test.car$class,predict.svm.radial) # Accuracy : 0.9971  


#----------------------------------------------------------------------------------
# SVM  Polynomial Kernel Implementation ( Kernel Implemetation)
# ((slow but more accurate)
#----------------------------------------------------------------------------------

grid <- expand.grid(
  C = c(0.25,0.5,0.75,1), degree = c(1,2,3,4),scale = c(0.0001,0.001,0.01,0.1))


model.svm.poly <- train(class ~ ., 
                          data = train.car,
                          method = "svmPoly",
                          tuneGrid = grid,
                          trControl = fitControl)

# The final values used for the model were degree = 3, scale = 0.1 and C = 0.75.


predict.svm.poly <- predict(model.svm.poly,test.car)

confusionMatrix(test.car$class,predict.svm.poly)  # Accuracy : 1

# ggplot(model.svm.poly) +
#   geom_smooth(aes(x = degree,y = Accuracy))

plot(model.svm.poly)
#----------------------------------------------------------------------------------
# Random Forest Implementation
#----------------------------------------------------------------------------------


grid <- expand.grid ( mtry = c(2,5,8,10,12,15))

model.svm.randomForest <- train(class ~ ., 
                        data = train.car,
                        method = "rf",
                        tuneGrid = grid,
                        trControl = fitControl)

# The final value used for the model was mtry = 10.
plot(model.svm.randomForest)

predict.svm.randomForest <- predict(model.svm.randomForest,test.car)

confusionMatrix(test.car$class,predict.svm.randomForest) # Accuracy : 0.939 



#----------------------------------------------------------------------------------
# Neural Network Implementation
#----------------------------------------------------------------------------------


grid <- expand.grid ( size = c(3,5,7,9,11), decay = c(0.0001,0.001,0.01,0.1)) 


model.svm.neuralNet <- train(class ~ ., 
                                data = train.car,
                                method = "nnet",
                                tuneGrid = grid,
                                trControl = fitControl)


model.svm.neuralNet
# The final values used for the model were size = 11 and decay = 0.01.

plot(model.svm.neuralNet)

predict.svm.neuralNet <- predict(model.svm.neuralNet,test.car)

confusionMatrix(test.car$class,predict.svm.neuralNet) #  Accuracy : 1  

# Through direct Algorithm package
library(nnet)
model.neuranet <- nnet(class ~ .,
                       data = train.car,
                       size = 11,
                       decay = 0.01)
pred.neuranet <- predict(model.neuranet,test.car,class())  # result may need to tranform to classification type
confusionMatrix(test.car$class,pred.neuranet)

#----------------------------------------------------------------------------------
# Conclusion
#----------------------------------------------------------------------------------
# There are multiple model above providing 100% accuracy, Any of those can be used for this classifciation problem
