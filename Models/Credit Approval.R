#--------------------------------------------------------------------------------
# Credit Approval - Classification Problem
#--------------------------------------------------------------------------------
# Libraries
library(funModeling)
library(caret)

#--------------------------------------------------------------------------------
# Read Data
#--------------------------------------------------------------------------------
# data URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"

credit <- read.table(
  file = url,sep = ",",stringsAsFactors = FALSE)

names(credit) = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","Class")
#--------------------------------------------------------------------------------
# Exploratory Data Analysis
#--------------------------------------------------------------------------------
# Delete observation that contains data with "?" for column A1
credit.clean <- subset(credit, credit$A1 != as.character("?"))
credit.clean <- subset(credit.clean, credit.clean$A4 != as.character("?"))
credit.clean$Class <- ifelse(credit.clean$Class == "+","Positive","Negative")
credit.clean$Class <- as.factor(credit.clean$Class)

credit.clean$A1 <- as.factor(credit.clean$A1)
credit.clean$A2 <- as.double(credit.clean$A2)
credit.clean$A4 <- as.factor(credit.clean$A4)
credit.clean$A5 <- as.factor(credit.clean$A5)
credit.clean$A6 <- as.factor(credit.clean$A6)
credit.clean$A7 <- as.factor(credit.clean$A7)

credit.clean$A9 <- as.factor(credit.clean$A9)
credit.clean$A10 <- as.factor(credit.clean$A10)

credit.clean$A12 <- as.factor(credit.clean$A12)
credit.clean$A13 <- as.factor(credit.clean$A13)

credit.clean$A14 <- as.integer(credit.clean$A14)


# Removing variables with high number of NA/zeros if any
data.summary <- df_status(credit.clean)
# A11 - 56.55% (380 observations)
# A15 - 42.41% ( 285 observations)
# A8  - 9.38%  ( 63 observations )
#  A3 -  1.93% ( 13 observations)      

summary(credit.clean)


cross_A1=cross_plot(credit.clean, str_input="A1", str_target="Class")
# A1 impact is not much on credit Class

cross_A4=cross_plot(credit.clean, str_input="A4", str_target="Class")
# A4 - There are only 2 observation of A4 - I, both these observation belongs to positiove.
# A4 - u observation ratio is almost equal to Class ratio and same for A4 - y. Seems that this variable has no impact on classification

cross_A5=cross_plot(credit.clean, str_input="A5", str_target="Class")
# Seems variable A4 and A5 are duplicating with different value. These are good cindidate for delete.

# Get rid off from unknow value observation for A6
credit.clean <- subset(credit.clean, credit.clean$A6 != as.character("?"))
 
cross_A6=cross_plot(credit.clean, str_input="A6", str_target="Class")
# this attributes has lot of variation, seems good condidate for model(***)

cross_A7=cross_plot(credit.clean, str_input="A7", str_target="Class")  
# seems there is no impact of feature on credit class

cross_A9=cross_plot(credit.clean, str_input="A9", str_target="Class") 
# seems there is no impact of feature on credit class

cross_A10=cross_plot(credit.clean, str_input="A10", str_target="Class") 
# seems there is no impact of feature on credit class


cross_A12=cross_plot(credit.clean, str_input="A12", str_target="Class") 
# seems there is no impact of feature on credit class

cross_A13=cross_plot(credit.clean, str_input="A13", str_target="Class") 
# seems there is no impact of feature on credit class

ggplot(data = credit.clean, aes(x = Class, y = A2)) +
  geom_boxplot(aes(fill = Class))  +
  facet_wrap( ~A6)

# Except few A6 category, boxplot shows mean value of A13 is more in positive case. Seems important features (****)

ggplot(data = credit.clean, aes(x = Class, y = A3)) +
  geom_boxplot(aes(fill = Class))  +
  facet_wrap( ~A6)


# In all the cases mean of A3 for positive is more compare to negative. Seems it is also an important feature

ggplot(data = credit.clean, aes(x = Class, y = A8)) +
  geom_boxplot(aes(fill = Class))  +
  facet_wrap( ~A6)



ggplot(data = credit.clean, aes(x = Class, y = A14)) +
  geom_boxplot(aes(fill = Class))  +
  facet_wrap( ~A6)
# Seems important but mean for negatives is higher almost in all cases compare to positive

ggplot(data = credit.clean, aes(x = Class, y = A15)) +
  geom_boxplot(aes(fill = Class))  +
  facet_wrap( ~A6)

# seems no impact of this variable
