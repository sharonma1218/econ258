#------------------------------------------------------------- #
# Lesson 13: Intro to Statistical Learning                     #
#                                                              #      
# An Exercise with Machine Learning Flavor                     #
# Predicting using OLS versus K-Nearest Neighbor               # 
#                                                              #
#                                                              #
#--------------------------------------------------------------# 

#install.packages("class")
library(class)

###################################################################
### Part 1: Create "Fake" data based on a function y = x + epsilon
###################################################################

# A. Create 1000 values of x such that: 
#    the next value of x is equal to the previous value of x plus 0.02.

x  = numeric(1000)
x[1] = -10

# Create code looping the values. 

for (t in 2:1000) {
  x[t] <- x[t-1] + 0.02
}

# B. Now let's create fake y that depends on x and the epsilon component. 

eps <- rnorm(1000, 0, 100) # normal distr(n = 1000, mean = 0, sd = 100) 
y <- 2525 + x^3 + eps # random shock on wages 

# Plot y on x

plot(y~x, las=1)

# Put in your fake data in a data frame (y,x, and eps)

....................

################################################
### Part 2: Sampling for training and test data
################################################

set.seed(1234) # To make sure we all get the same answers.

# Create a learning and sample set

learning_sample <- subset(fake_data, random_unif<=...)
test_sample <- ... 

# Plot the y on x for both the learning and test sample

plot(y~x,, data=learning_sample, las=1)
plot(y~x, data=testing_sample, las=1)

#######################################################
### Part 3: Linear Regression and K-nearest neighbour
#######################################################

# Compare a linear model (parametric) versus a k-nearest neighbour (non-parametric)

# A. Estimate the linear regression of y on x using the learning sample

ols_model <- lm(y~x, data=learning_sample)
View(ols_model)

test_model <- 
View(test_model)

# B. Predict y using the linear regression model you estimated before for the learning sample

learning_sample$predict_y <- predict(ols_model)

# C. Now predict the y using the linear regression model you estimated for the test sample  

test_sample$predict_y <- predict(ols_model, newdata=test_sample)

# D. Calculate the mean square errors for both prediction

MSE_ols_learn <- mean((learning_sample$y-learning_sample$predict_y)^2)
MSE_ols_test <- mean((test_sample$y-test_sample$predict_y)^2)

# E. Plot the y for both (in a separate graph is fine)

plot(predict_y~x, data=learning_sample, las=1)
plot(predict_y~x, data=test_sample, las=1)

# F. The code below estimates a model using k nearest neighbor, and predicts y

kkn_predict <- knn(train = data.frame(learning_sample$x), test = data.frame(test_sample$x), cl = learning_sample$y, k = 1)
kkn_predict_y <- as.numeric(as.character(kkn_predict))
plot(kkn_predict_y ~ test_sample$x, las = 1)

# G. Recalculate the mean square error for the prediction with k nearest neighbor

MSE_knn_test <- mean((test_sample$y-knn_predict_y)^2)
MSE_knn_test
MSE_ols_test
