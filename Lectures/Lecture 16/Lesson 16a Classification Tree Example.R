################################################################################
# Lesson 16b:  Classification Trees
#
# Econ 258
#
# Fitting Classification Trees
# Based on code provided by James et. al Intro to Statistical Learning (2013)
################################################################################

# Important: delete the data in global environment each time you need to rerun. 

library(tree)
library(ISLR)
attach(Carseats)

# Suppose you are employed as an analyst for a company who sell child car seats. 
# You want to understand what determines sales of child car seats. 
# Sales are considered high if a store has more than 8000 sales in that location. 

help("Carseats")
High <- as.factor(ifelse(Sales<=8,"No","Yes"))
Carseats <- data.frame(Carseats,High)

# Fit in the data to a classification tree
tree.carseats <- tree(High~.-Sales,Carseats) # Use every variable in the data except for Sales. 
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
# Observe in the summary above that we misspecify 36 out of 400 observation. 
# What is wrong with the exercise above? We did not use a training and a test sample. 

# Step 1) Create training and test sample. 
----------------------------------------------
  
# Step 2) Fit a classification tree on the training sample.  
----------------------------------------------
  
# Step 3) Use the model to predict on the test sample. 
# Use the predict command, add the input type="class" in your command. 
-----------------------------------------------
  
# Step 4) Create a 2x2 table showing the actual test high/low sales versus predicted high low sales
# What percentage of the observation did the model predict correctly? 
-----------------------------------------------
  
# Step 5) Let's see if we can do better with pruning. 
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) # Need to use Fun=prune.misclass for classification tree
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")

# Prune the tree for what you think will be the best tree size. 
# Check whether you are able to predict more from the test sample. 
-----------------------------------------------
  