################################################################################
# Lesson 17:  Neural Network Practice
#             Detecting Counterfeit Money 
#
#
# Project from Schmuller (2018) 
# Get other ML datasets from https://archive.ics.uci.edu/ml/datasets.php
################################################################################

################################################################################################
# Project 1: Train an algorithm to predict species of iris flowers based on its characteristics
################################################################################################

#install.packages("nnet")
#install.packages("NeuralNetTools")
library(nnet)
library(NeuralNetTools)
data(iris)
summary(iris)

set.seed(810)

train      <- sample(1:nrow(iris), 100) # train random sample of sample 
iris.train <- iris[train,] # train data
iris.test  <- iris[-train,] # test data

# Training a neural network with 4 hidden layers
nn.iris <- nnet(Species ~ Petal.Length + Petal.Width, iris.train, size=4) # all of the neural network
#nn.iris <- nnet(Species ~ ., iris.train, size=4)

summary(nn.iris)

plotnet(nn.iris)

predictions <-  predict(nn.iris, iris.test, type = "class") # now we will predict our test data; don't forget "type = "class"" b/c categorical

table(iris.test$Species, predictions) # very good prediction 

olden(nn.iris) # Variable importance is based on connection weights. Shows that petal width is more important to predict neural network / iris type. 

################################################################################
# Project 2: Train an algorithm to predict whether the currency is fake or not
################################################################################

setwd("~/Downloads/Econ 258 R/Lesson 17 Neural Network")

banknote <-read.table("lesson_17_data_banknote_authentication.txt", 
                      header=FALSE, sep=",", 
                      col.names = c("variance", "skewness", "kurtosis", "enthropy", "class"))

banknote$class <- as.factor(banknote$class)

# Step 1: Look at data. Plot two variables by class (1 is legit 0 is fake), and see if you can see a pattern. 
#         Enthropy versus Kurtosis for example and see whether it is easy to separate fake versus real money. 
#         You can try different variables. 

ggplot(banknote, aes(kurtosis, enthropy)) +
  geom_point() +
  facet_wrap(~class)

ggplot(banknote, aes(skewness, variance)) + 
  geom_point() + 
  facet_wrap(~class)
# the variances are difference between fake and real 

# Step 2: Create a training and test set. 

train2 <- sample(1:nrow(banknote), 100) 
banknote.train <- banknote[train,]
banknote.test <- banknote[-train,]

# Step 3: Create a neural network to predict legit (1) versus fake (0) currency with 3 hidden layers. 
#         Summarize the model and graph. 

nn.banknote <- nnet(class ~ variance + skewness + kurtosis + enthropy, banknote.train, size=3)
summary(nn.banknote)
plotnet(nn.banknote)

# Step 4: Use the model to predict information in the test data. Since its a classification, do not forget to specify type="class". 
#         Then plot the model. 

banknote.predictions <- predict(nn.banknote, banknote.test, type = "class")
plotnet(banknote.predictions)



# reference 
table(iris.test$Species, predictions) # very good prediction 
olden(nn.iris)

# real 



# Step 5: Evaluate your model. 
#         Create a table showing actual versus predicted class variable.
#         What percentage are you able to predict correctly?


# Step 6: Graph variable importance graph. Which variable is most important? 

