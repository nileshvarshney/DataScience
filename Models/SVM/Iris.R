library(caret)

# Read data and divide data in training and test set
df.data <- iris

set.seed(2017)
trainIndex <- createDataPartition(
  iris$Species, 
  p = .8, 
  list = FALSE, 
  times = 1
  )

train.set <- df.data[trainIndex,]
test.set <- df.data[-trainIndex,]

table(train.set$Species) # 40 each
table(test.set$Species)  # 10 each
table(df.data$Species)   #  50 each

#------------------------------------------------------------
#  SVM Linear
#------------------------------------------------------------

ctl <- trainControl(
  method = "repeatedcv",
  number = 5,    ## 10-fold CV...
  repeats = 5,   ## repeated ten times
  classProbs = TRUE
  )


grid <- expand.grid(C = c(0.25,0.5,0.75,1,1.25,1.5))

svm_linear_model <- train(Species ~ ., 
                          data= train.set,
                          method = "svmLinear",
                          preProcess = c("center","scale"),
                          tuneGrid = grid,
                          trControl = ctl
                         )

svm_linear_model
# The final value used for the model was C = 1.25
#------------------------------------------------------------
#  SVM Radial
#------------------------------------------------------------
grid <- expand.grid(C = c(0.25,0.5,0.75,1,1.25,1.5),sigma = c(0.001,0.01,0.1))


svm_radial_model <- train(Species ~ ., 
                          data= train.set,
                          method = "svmRadial",
                          preProcess = c("center","scale"),
                          tuneGrid = grid,
                          trControl = ctl
)

svm_radial_model
# The final values used for the model were sigma = 0.1 and C = 1.5

#------------------------------------------------------------
#  SVM Poly
#------------------------------------------------------------

grid <- expand.grid(C = c(0.25,0.5,0.75,1,1.25,1.5),
                    degree = c(1,2,3),
                    scale = c(0.001,0.01,0.1))


svm_poly_model <- train(Species ~ ., 
                          data= train.set,
                          method = "svmPoly",
                          preProcess = c("center","scale"),
                          tuneGrid = grid,
                          trControl = ctl
)

svm_poly_model
# The final values used for the model were degree = 3, scale = 0.1 and C = 1.25.

#----------------------------------------------------------
# Compare All three models ( Resample )
#----------------------------------------------------------
comparisions <- resamples(list(linear = svm_linear_model,
                               radial = svm_radial_model,
                               poly = svm_poly_model)) 


summary(comparisions)

#----------------------------------------------------------
# predict
#----------------------------------------------------------
pred_linear <- predict(svm_linear_model,test.set)
confusionMatrix(pred_linear,test.set$Species)

pred_radial <- predict(svm_radial_model,test.set)
confusionMatrix(pred_radial,test.set$Species)

pred_poly <- predict(svm_poly_model,test.set)
confusionMatrix(pred_poly,test.set$Species)

#-----------------------------------------------------
# KNN through Carat package
#-----------------------------------------------------
grid <- expand.grid(k = c(1:20))

knn_model <- train(Species ~ ., 
                   data= train.set,
                   method = "knn",
                   preProcess = c("center","scale"),
                   tuneGrid = grid,
                   trControl = ctl
)

summary(knn_model)
# The final value used for the model was k = 5

pred_knn <- predict(knn_model,test.set)
confusionMatrix(pred_knn,test.set$Species)

#-----------------------------------------------------
# Random forest 
#-----------------------------------------------------
library(randomForest)
model.random <- randomForest(Species ~.,data = train.set)

pred.random <- predict(model.random ,test.set)

confusionMatrix(test.set$Species,pred.random)















