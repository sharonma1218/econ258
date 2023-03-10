################################################################################
# Lesson 16b:  Bagging and Random Forest
#
#
# Econ 258 Data Analytics with R
#
# Based on code provided by James et. al Intro to Statistical Learning (2013)
################################################################################


####################################
#
# Bagging and Random Forest ########
# 
####################################

# Back to understanding housing prices in suburban boston

library(randomForest)
library(MASS)
set.seed(1)

# You can do both bagging and random forest using the same command. 
# For bagging, you just need to specify that you will use all the variables. Thus mtry = 13.

train <- sample(1:nrow(Boston), nrow(Boston)/2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

# Use your model to predict the test data 
yhat.bag = predict(bag.boston,newdata=Boston[-train,])

# Actual test data prices
boston.test=Boston[-train,"medv"]

# Compare them side by side
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
# You can compare this with what we got last class using a regular tree and pruning. 

# You can also decide how many number of trees you want to build. 
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2) 

# To do random forest, choose a smaller mtry. 
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)  
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2) 
# turns out mtry=3 (3 variables) is the best

# Check variable importance and graph it.
importance(rf.boston) # if i take out this variable, how much would my mean square error increase?
varImpPlot(rf.boston) # Number of rooms and lower status of the population matters a lot. 

# Look at partial dependence plots.
Boston_train <- Boston[train,]
partialPlot(rf.boston, pred.data = Boston_train, x.var = "lstat") 
partialPlot(rf.boston, pred.data = Boston_train, x.var = "rm") 

