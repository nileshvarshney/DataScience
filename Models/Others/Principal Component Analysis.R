#--------------------------------------------------------------------#
#--------------  Principal components analysis ----------------------#
#--------------------------------------------------------------------#
# Principal components analysis (PCA, for short) is a variable-reduction technique. This is one of the way to remove the variable where multicollinearity found. This is also used to predictor variable that has no or very less effect on models.

# Read data from UCI Machine Learning Repository
airfoil <- read.table(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat")

# Add variable names
names(airfoil) <- c(
  "Frequency",
  "Angle",
  "ChordLength",
  "Velocity",
  "Thickness",
  "Pressure.level"
)

head(airfoil)                 

# Apply linear regression of sample data
model <- lm(Pressure.level ~ ., data = airfoil)
# Model shows that all 5 predicators are statistically sognificant. Lets perform principal components analysis.

# Summary of the model
summary(model)

# Remove dependent variable from the dataset
airfoil.X <- airfoil[,-6]

# Perform principal component analysis
airfoil.X.pca <- prcomp(
  airfoil.X, 
  center = TRUE, 
  scale = TRUE)


plot(airfoil.X.pca,type = "l") # This is sometime called scree plot. This shows the variance explained by each principal componets

summary(airfoil.X.pca)
# Shows PC1 - explaining 42.19 % , PC2 is explaining 22.48% aand PC3 explaining 18.33% variance. Normal approach is performed to consider PC those are covering 95 to 99% of variance and rest of the variable can safely be get rid off.

# lets see the rotation matrix
print(airfoil.X.pca$rotation)

# Frequency is contributing 27.83% in PC1 and just 0.62 % in PC2. Similarly other variables are contribbutung in different principal components.

# Lets see the eigen value of dataset
eigen(cor(airfoil.X))$value

# Lets check the variance of each principal components
diag(var(airfoil.X.pca$x))

# Variance of each principal component is same as eigen value of original data set.

# Lets double check the new principal components are orthogonal.( Variables are statistically indepent)
cor(airfoil.X.pca$x)
# yes they are statistically independent.


# Lets see the principal component regression
airfoil.pca <- cbind(data.frame(airfoil.X.pca$x ),airfoil[,6])
colnames(airfoil.pca)[6] <- "Pressure.level"

head(airfoil.pca)

# Now look the correlation matrix
cor(airfoil.pca)[,6]   # it shows that PC4 is highly correlated and then PC5. PC1 is least correlated.

# fit the PC data in linear model
model.pca <- lm(Pressure.level ~ ., data = airfoil.pca)

# Lets see the summary of model
summary(model.pca)  # Model is almost same in both the cases. All PCs are statistically significant.

# Lets convert back the model coefficient back to original variables
betas <- airfoil.X.pca$rotation %*% model.pca$coefficients[-1]  # This will return the model coefficient in original variable terms
betas

# Lets compare both the models through ANOVA
anova(model,model.pca)  # There is no statistically significant difference.













