#------------------------------------------------------------------------------------------#
#------------- Detecting mullticollenearity -----------------------------------------------#
#------------------------------------------------------------------------------------------#

library(psych)
iris.num <- data.frame(scale(iris[,1:4]))
pairs.panels(scale(iris.num))

# Calculate the Correlation  matrix
iris.cor <- cor(iris.num)
iris.cor

# There is strong chances of mullticollenearity exists between variables as correlation between variables is 0.96 to 0.82. COR shows correlation between two variables. Sometime mullticollenearity is more complex. There are couple of other way to find mullticollenearity.

# Eigen System Analysis
eigen(iris.cor)$value

# if all values are almost same then there is no mullticollenearity exists. if these values are different then mullticollenearity exists.

# Ratio of max and min eigen value 
max(eigen(iris.cor)$value)/min(eigen(iris.cor)$value)  # 140.88

# Rule of thumb if this ratio is greater then 100 then there is mullticollenearity exists between variables. Same can be achieved by kappa

kappa(iris.cor,exact = TRUE) # 140.88


# Second method to check mullticollenearity through VIF ( variation inflation Factor)
model <- lm(Sepal.Length ~ ., data = iris.num)

library(car)

# Are vif greater than 5 or 10 ?
vif(model)  # 1.270815    15.097572    14.234335

# Rule of thumb is that if vif value is more than 4-5 then there is high chance of mullticollenearity. This need to analysis further

# Is mean of VIF is greater than 1?
mean(vif(model))  # 10.20091. It is clear indication of mullticollenearity.

# All three previous test are confirming mullticollenearity exists.


#------------------------------------------------------------------------------------------#
#------------- Detecting mullticollenearity Treatment -------------------------------------#
#------------------------------------------------------------------------------------------#
# Ridge Regression is a technique for analyzing multiple regression data that suffer from multicollinearity. When multicollinearity occurs, least squares estimates are unbiased, but their variances are large so they may be far from the true value.

library(MASS)
lm_seq <- seq(0,10,0.001)
model.ridge <- lm.ridge(Sepal.Length ~ ., data = iris.num,lambda = lm_seq )

# Plot the model
plot(model.ridge)

# lets get best value of lamda
select(model.ridge)
# modified HKB estimator is 0.05450435 
# modified L-W estimator is 0.1691823 
# smallest value of GCV  at 0.073 ---will use this

# generalized Cross validation GCV Plot
plot(lm_seq, model.ridge$GCV,type = "l", xlab = "Expression Lamda", ylab = "GCV")

lm.ridge(Sepal.Length ~ ., data = iris.num,lambda = 0.3 )
lm.ridge(Sepal.Length ~ ., data = iris.num,lambda = 0)








