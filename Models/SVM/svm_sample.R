library(caret)

# Load the data and create training set and test sample.
data(segmentationData, package = "caret")  # caret package data
df.data <- segmentationData

set.seed(2017)
train.sample <- createDataPartition( df.data$Class,
                                  p = 0.7,
                                  list = FALSE)

train.set <- df.data[train.sample,-c(1:2)]
test.set <- df.data[-train.sample,-c(1:2)]

prop.table(table(df.data$Class)) # 0.6438831 0.3561169 
prop.table(table(train.set$Class)) # 0.6435644 0.3564356 
prop.table(table(test.set$Class)) # 0.6446281 0.3553719

# Setup Train control
set.seed(2017)
ctl <-  trainControl(method = "repeatedcv", # repeaat k fold validatation
                     repeats = 3, # 5 repeat of cv
                     classProbs = TRUE
                    # summaryFunction = "twoClassSummary"
)
                     
grid <- expand.grid(C = c(0.25,0.5,0.75,1,1.25,1.5))

# names(getModelInfo())  # get Model Information
# Train and tune svm
svm_linear_model <- train(Class ~ ., data = train.set,
                          method = "svmLinear",
                          preProcess = c("center", "scale"),
                          trControl = ctl,
                          tuneGrid = grid #,
                      #    metric = "ROC"
)
                          
svm_linear_model   
# The final value used for the model was C = 0.25.

#------------------------------------------------------
# SVM Radial
#------------------------------------------------------

grid <- expand.grid(sigma = c(0.01,0.015,0.02,0.025,0.03),
                    C = c(0.25,0.5,0.75,1,1.25,1.5)
)
# Train and tune svm Radial Model
svm_radial_model <- train(Class ~ ., data = train.set,
                          method = "svmRadial",
                          preProcess = c("center", "scale"),
                          tuneGrid = grid,
                          trControl = ctl
)

svm_radial_model 
# The final values used for the model were sigma = 0.02 and C = 1.25.

#------------------------------------------------------
# SVM Poly
#------------------------------------------------------
# Train and tune svm Poly Model
grid <- expand.grid(degree = c(1,2,3),
                    scale = c(0.001,0.01,0.1),
                    C = c(0.25,0.5,0.75,1,1.25,1.5)
                    )
svm_poly_model <- train(Class ~ ., data = train.set,
                          method = "svmPoly",
                          preProcess = c("center", "scale"),
                          tuneGrid = grid,
                          trControl = ctl,
                          tuneLength = 2
)

svm_poly_model

#####################################
  comparision <- resamples(list(linear = svm_linear_model,
                                radial = svm_radial_model) 
  )
