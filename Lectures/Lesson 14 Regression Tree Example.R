################################################################################
# Lesson 14:  Regression Trees
#
# Econ 258 Data Analytics with R
#
# Fitting Regression Trees
# Based on code provided by James et. al Intro to Statistical Learning (2013)
################################################################################

# Question: Create a model to predict housing values in suburbs of Boston

#install.packages(c("ISLR", "tree"))
library(tree)
library(ISLR)
library(MASS) # To upload the dataset Boston 
help(Boston)   
set.seed(1)

full_data <- Boston

# We always need a training sample and a test sample
# In a previous exercise, I assigned random numbers between 0-1 to the data and select training/test based on a cutoff
# You can also use the code sample() to randomly get half the data from Boston. (Careful, train gives the specific number of observations randomly chosen, not the data) 

# the following samples a random half from the full data set 
train_index <- sample(1:nrow(Boston), nrow(Boston)/2)
train_data1 <- full_data[train_index,]
test_data1  <- full_data[-train_index,]   

# Another way to split the sample
train_index2  <- runif(nrow(full_data))
train_data2   <- subset(full_data, train_index2<=0.5) 
test_data2    <- subset(full_data, train_index2>0.5)

# We want to predict medv (median value of owner-occupied homes in $1000s)
# The . after medv~, specifies that you want to use all variables in the data set
# Since previously we created train, which is subset of data observations for training, 
# we can specify it to the subset and give the entire data boston the the tree command. 

tree.boston  <- tree(medv~., data = full_data, subset=train_index) # i want to predict median val of house; the "." means the computer tells us which variables matter
tree.boston2 <- tree(medv~., data = train_data1) 

# Both code above are fine. Exposing you to multiple ways to code this.  

summary(tree.boston)
summary(tree.boston2) # out of all the variables, the computer decided rm (avg number of rooms), lstat (lower stat of pop), crim, and age (of building) were the most important in determining median val of house 

# Observe that out of all the variables we gave it, only 4 was used in the final tree. 
# rm (rooms per dwelling), lstat (% of lower status of population), crim (per capita crime rate), age (proportion of owner occupied units built prior to 1940)
# We can plot the tree below. 

plot(tree.boston)
text(tree.boston,pretty=0)

# Let's do cross-validation on the test sample to choose how to prune the tree. 
cv.boston=cv.tree(tree.boston) # testing things in different samples 
plot(cv.boston$size,cv.boston$dev,type='b') # if i have a tree that has 6 nodes vs 7 nodes, performance wise, it's not doing that much better. so maybe we don't need 7 nodes; just 6 is fine. 

# Let's do tree pruning.
prune.boston=prune.tree(tree.boston,best=6)
plot(prune.boston)
text(prune.boston,pretty=0)

# How well did our model do in predicting y?
yhat <- predict(tree.boston, newdata=Boston[-train_index,])
boston.test <- Boston[-train_index,"medv"]
plot(yhat,boston.test, xlim=c(0,50), ylim=c(0,50)) 
abline(0,1) # if it's a perf model, data points should fall on 45 deg line 
mean((yhat-boston.test)^2)

yhat_prune <- predict(prune.boston,newdata=Boston[-train_index,])
plot(yhat_prune,boston.test, xlim=c(0,50), ylim=c(0,50))
abline(0,1) 
mean((yhat_prune-boston.test)^2) # improvement; lower RSE 

# Some other graphing options 

library(partykit)
library(rpart)

rpart.tree <- rpart(medv~., data = full_data, subset=train_index)
plot(rpart.tree, uniform=TRUE)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")
rparty.tree <- as.party(rpart.tree)
rparty.tree

png("regression_tree.png", width = 1000, height = 1000) 

plot(rparty.tree)

dev.off()
