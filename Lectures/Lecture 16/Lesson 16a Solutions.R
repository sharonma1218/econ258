################################################################################
# Lesson 16b:  Classification Trees (Solutions)
#
# Fitting Classification Trees
# Based on code provided by James et. al Intro to Statistical Learning (2013)
################################################################################

# Important: delete the data in global environment each time you need to rerun. 

library(tree)
library(ISLR)
attach(Carseats)

# Suppose you are employed as an analyist for a company who sell child car seats. 
# You want to understand what determines sales of child car seats. 
# Sales are considered high if a store has more than 8000 sales in that location. 

help("Carseats")
High <- as.factor(ifelse(Sales<=8,"No","Yes"))
Carseats <- data.frame(Carseats,High)

# Fit in the data to a classification tree
set.seed(12)

tree.carseats <- tree(High~.-Sales,Carseats) # Use every variable in the data except for Sales. 
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

# Observe in the summary above that we misspecify 36 out of 400 observation. 
# What is wrong with the exercise above? We did not use a training and a test sample. 

# Step 1) Create training and test sample. 
train <- sample(1:nrow(Carseats), 200)
Carseats.test <-  Carseats[-train,]
High.test <- High[-train]

# Step 2) Fit a classification tree on the training sample.  
tree.carseats <- tree(High~.-Sales,Carseats,subset=train)

# Step 3) Use the model to predict on the test sample. 
# Use the predict command, add the input type="class" in your command. 
tree.pred <- predict(tree.carseats,Carseats.test,type="class")

# Step 4) Create a 2x2 table showing the actual test high/low sales versus predicted high low sales
# What percentage of the observation did the model predict correctly? 
table(tree.pred,High.test)
(99+54)/200*100 # % predicted correctly

# Step 5) Let's see if we can do better with pruning. 
set.seed(3)
cv.carseats <- cv.tree(tree.carseats,FUN=prune.misclass) # Need to use Fun=prune.misclass for classification tree
names(cv.carseats)
cv.carseats
plot(cv.carseats$size,cv.carseats$dev,type="b")

  # Find index of tree with least error
  min_idx <-  which.min(cv.carseats$dev)
  cv.carseats$size[min_idx] # the plot & this shows that the best branch is at 8 
  
# Prune the tree for what you think will be the best tree size and check whether you are able to predict more.
prune.carseats=prune.misclass(tree.carseats,best=8)
plot(prune.carseats) 
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(102+52)/200*100 # % predicted correctly; slightly better

# Trying other tress (although we know any other would be worst trees)
prune.carseats=prune.misclass(tree.carseats,best=6)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(103+45)/200*100 # % predicted correctly
