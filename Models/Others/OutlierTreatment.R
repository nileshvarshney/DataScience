#------------------------------------------------
# Outlier Treatment
# -----------------------------------------------
# Injecting outlier in test data
car1 <- cars1 <- cars[1:30, ]
car_outlier <- data.frame(speed = c(19,19,20,21,20), dist=c(190, 186, 210, 220, 218)) 
car2 <- rbind(car1,car_outlier)

plot(car1$speed,car1$dist)
abline(lm(dist ~ speed, data = car1),col= "blue",lwd = 3,lty = 2)

# Without Outlier
ggplot(data = car1,aes(x = speed, y = dist)) +
  geom_point(color = "red",size = 3) +
  geom_smooth(method = "lm",color = "blue",linetype="dashed") +
  ggtitle("Plot without outliers")

# With Outliers
ggplot(data = car2,aes(x = speed, y = dist)) +
  geom_point(color = "red",size = 3) +
  geom_smooth(method = "lm",color = "blue",linetype="dashed") +
  ggtitle("Plot with outliers")

#------------------------------------------------
# Detect Outliers
#------------------------------------------------
# Univariate approach (observations that lie outside 1.5 times IQR)
Outliers_value <- boxplot.stats(car2$dist)$out
Outliers_value

boxplot(mtcars$wt)
outlier_value <- boxplot.stats(mtcars$wt)$out
outlier_value

# Bivariate approach
boxplot(wt ~ cyl, data = mtcars)
boxplot.stats(wt ~ cyl, data = mtcars)  

# Can see that there are few outlier for cyl 8. Which indicate view cars is overweight for cyclider. Either this value is wrong or they are the extreame case. Can not consider to remove them as they are more towards the extreame case rather than outliers

# Multivariate Model Approach (regression Model)
model.lm <- lm(mpg ~ ., data= mtcars)

plot(model.lm)
cooksd <- cooks.distance(model.lm)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
# In general use, those observations that have a cook’s distance greater than 4 times the mean may be classified as influential. This is not a hard boundary.
cooksd[cooksd > 4 * mean(cooksd,na.rm = T)]

influential <- names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]
head(mtcars[influential,])

#------------------------------------------------
# Outliers Test
#------------------------------------------------
car::outlierTest(model.lm)

# Test suggest that "Ford Pantera" seams outlier 

# Through Outlier Package ( does not give sense sometime)
outliers::outlier(mtcars$wt)
outliers::outlier(mtcars$mpg)
outliers::outlier(mtcars$qsec)
outliers::outlier(mtcars$cyl)
outliers::outlier(mtcars$hp)

#------------------------------------------------
# Outliers Treatment
#------------------------------------------------
# 1. Imputation with mean / median / mode.
# 2. we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie above the upper limit, with the value of 95th %ile.

dplyr::tbl_df(iris)

head(iris)



