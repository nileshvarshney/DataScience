#------------------------------------------------------------
# One Sample t-Test
#------------------------------------------------------------
# Purpose : it is a parametric test used to test if the mean of a sample from a normal distribution could reasonably be a specific value.

set.seed(100)
x <- rnorm(50, mean = 10, sd = 0.5)
t.test(x, mu=10)

# One Sample t-test

# data:  x
# t = 0.70372, df = 49, p-value = 0.4849
# alternative hypothesis: true mean is not equal to 10
# 95 percent confidence interval:
#   9.924374 10.157135
# sample estimates:
#   mean of x 
# 10.04075 


# The p-Value is not less than significance level of 0.05, therefore the null hypothesis that the mean=10 cannot be rejected. Also note that the 95% confidence interval range includes the value 10 within its range. So, it is ok to say the mean of ‘x’ is 10, especially since ‘x’ is assumed to be normally distributed.