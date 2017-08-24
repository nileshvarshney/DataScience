setwd("~/Google Drive/Gits/DataScience/Models/Credit")
library(ggplot2)

## Read CSV file 
credit <- read.csv("credit.csv")
str(credit)

## Exploratory Data Analysis
table(credit$checking_balance)
table(credit$credit_history)
table(credit$other_credit)
table(credit$housing)
table(credit$job)
table(credit$phone)
table(credit$default)
summary(credit$months_loan_duration)

# Plots
ggplot(data = credit) +
  geom_histogram(aes(months_loan_duration) , fill = "blue") +
  scale_x_continuous(name = "Loan Duration (Months)", 
                     breaks = seq(0,72,3), 
                     limits = c(0,72)) +
  scale_y_continuous(name = "No of Loan Accounts") +
  ggtitle("Loan Month Duration Counts")

ggplot(data = credit) +
  geom_histogram(aes(amount) , fill = "orange",binwidth = 100) +
  scale_x_continuous(name = "Loan Amount",
                     breaks = seq(0,15000,1000),
                     limits = c(0,15000)) +
  scale_y_continuous(name = "No of Loan Accounts") +
  ggtitle("Loan Amount wise Counts")

ggplot(data = credit,aes(age,amount)) +
  geom_point(alpha = 1/5) +
  xlim(18,quantile(credit$age,0.99)) +
  ylim(0,quantile(credit$amount,0.99)) +
  xlab("Age") +
  ylab(" Loan Amount") +
  ggtitle("Age Wise Loan excludes top 1% from age and amounts")


set.seed(123)
train.sample = sample(1000,900)
train.set <- credit[train.sample,]
test.set <- credit[-train.sample,]
prop.table(table(train.set$default))  ## training set propotion
prop.table(table(test.set$default))  ## test set propotion

#-----------------------------------------------------------------------
# Build Model on training set of data using Decision Tree Package C5.0
#-----------------------------------------------------------------------
# Implementing Decision tree through Carat Package which will internally use C5.0 Package
library(caret)
# modelLookup("C5.0")

set.seed(2017)
model.c50 <- train( default ~ ., data = train.set, method = "C5.0")
# The final values used for the model were trials = 20, model = tree and winnow = FALSE.

## prediction
predict.c50 <- predict(model.c50, test.set)
table(predict.c50,test.set$default)
head(predict(model.c50, test.set,type = "prob"))
head(test.set$default)

# customorize the tunning process
ctrl <- trainControl(
  method = "cv",     # k-fold validation
  number = 10,      # 10 folds
  selectionFunction = "oneSE"
)

grid <- expand.grid(
  model = "tree",
  trials = c(1,5,10,15,20,25,30,35),
  winnow = "FALSE"
)

grid
set.seed(2017)

model.c50.custon <- train(default ~ ., 
                          data = train.set, 
                          method = "C5.0",
                          metric = "Kappa",
                          trControl = ctrl,
                          tuneGrid = grid)

model.c50.custon
#he final values used for the model were trials = 5, model = tree and winnow = FALSE.

# This model is giving best result in  5 trials while previous one providing best result in 20 trials. This may be due to selection Function OneSE used which is overiding the default best selection by the model.

predict.c50.custon <- predict(model.c50.custon, test.set)
table(predict.c50.custon,test.set$default)
head(predict(model.c50.custon, test.set,type = "prob"))
head(test.set$default)


#---------------------------------------------------------------------------------
# Ensembles
#---------------------------------------------------------------------------------
# install.packages("ipred")
library(ipred)

set.seed(2017)
mybag <- bagging(default ~ ., data = train.set, nbagg = 25)

bagging_pred <- predict(mybag, test.set)
table(bagging_pred,test.set$default)

# Bagging through Caret package
set.seed(2017)
ctrl <- trainControl(
  method = "cv",
  number = 10
)

bagging_carat <- train(
  default ~., data = train.set,
  method = "treebag",
  trControl = ctrl
)

pred_bagging_carat <- predict(bagging_carat, test.set)
table(pred_bagging_carat,test.set$default)

















































