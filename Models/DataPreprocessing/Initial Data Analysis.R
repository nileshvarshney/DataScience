# Data Cleaning
# Variable importance analysis
# Assessing model performance

library(funModeling)
heart_disease <- heart_disease

# quantity of zeros
# quantity of NA
# type: factor or numeric
# quantity of unique values

# Removing variables with high number of NA/zeros
data.status <- df_status(heart_disease)

# Removing variables with 60% of zero values (q_zeros - quanitity, p_zeros - percent)
var_to_remove = subset(data.status,data.status$p_zeros > 60)
var_to_remove$variable
heart_disease.2 <- heart_disease[,!(names(heart_disease) %in% var_to_remove$variable)]
str(heart_disease.2)


# Odering data by percent of zeros
data.status[order(-data.status$p_zeros),]  # Decending order

# Target variable must have only 2 values. If it has NA values, they will be removed.


# Is gender co-related with the heart desease
cross_gender=cross_plot(heart_disease, str_input="gender", str_target="has_heart_disease")

# Gender variable seems to be a good predictor, since the likelihood of having heart disease is different given the female/male groups.

# Crossing with numerical variables
cross_plot(heart_disease, str_input="max_heart_rate", str_target="has_heart_disease") 

# Example 3: Noise reducing
# 
# Converting variable max_heart_rate into a one of 10 bins:
#   
heart_disease$max_heart_rate_2=equal_freq(var=heart_disease$max_heart_rate, n_bins = 10)  
cross_plot(heart_disease, str_input="max_heart_rate_2", str_target="has_heart_disease")  


# Importance variable analysis with cross_plot
# At a first glance, max_heart_rate_2 shows a negative and linear relationship, however there are some buckets which add noise to the relationship. For example, the bucket (141, 146] has a higher heart disease rate than the previous bucket, and it was expected to have a lower. This could be noise in data.
# 
# Key note: One way to reduce the noise (at the cost of losing some information), is to split with less bins:
#   
heart_disease$max_heart_rate_3=equal_freq(var=heart_disease$max_heart_rate, n_bins = 5)  
cross_plot(heart_disease, str_input="max_heart_rate_3", str_target="has_heart_disease")  


# Importance variable analysis with cross_plot
# Conclusion: As it can be seen, now the relationship is much clean and clear. Bucket ‘N’ has a higher rate than ‘N+1’, which implies a negative correlation.

# 

# Example 4: cross_plot on multiple variables
# 
# Imagine you want to run cross_plot for several variables at the same time. To achieve this goal you define a list of strings containing all the variables to use as input in the cross_plot, and then, call the function massive_cross_plot.
# 
# If you want to analyze these 3 variables:
#   
vars_to_analyze=c("age", "oldpeak", "max_heart_rate")  
massive_cross_plot(data=heart_disease, str_target="has_heart_disease", str_vars=vars_to_analyze)  


# Final notes:
#   
# Correlation does not imply causation
# cross_plot is good to visualize linear relationships, giving it a hint on non-linear relationships.
# Cleaning the variables help the model to better modelize the data.
# Part 3: Assessing model performance
# 
# Overview: Once the predictive model is developed with training data, it should be compared with test data (which wasn’t seen by the model before). Here is presented a wrapper for the ROC Curve and AUC (area under ROC) and the KS (Kolmogorov-Smirnov).
# 
# Creating the model
# 
# ## Training and test data. Percentage of training cases default value=80%.
index_sample=get_sample(data=heart_disease, percentage_tr_rows=0.8)
# 
# ## Generating the samples
data_tr=heart_disease[index_sample,]  
data_ts=heart_disease[-index_sample,]
# 
# 
# ## Creating the model only with training data
fit_glm=glm(has_heart_disease ~ age + oldpeak, data=data_tr, family = binomial)
# ROC, AUC and KS performance metrics
# 
# ## Performance metrics for Training Data
model_performance(fit=fit_glm, data = data_tr, target_var = "has_heart_disease")  
# Model performance, ROC, AUC and KS
# ## Performance metrics for Test Data
model_performance(fit=fit_glm, data = data_ts, target_var = "has_heart_disease") 
