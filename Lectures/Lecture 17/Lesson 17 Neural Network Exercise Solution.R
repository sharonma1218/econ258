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
library(ggplot2)

set.seed(810)

train      <- sample(1:nrow(iris), 100)
iris.train <- iris[train,]
iris.test  <- iris[-train,]

# Training a neural network with 4 hidden layers
nn.iris <- nnet(Species ~ Petal.Length + Petal.Width, iris.train, size=4)
#nn.iris <- nnet(Species ~ ., iris.train, size=4)

summary(nn.iris)

png("graph_neural_network.png", width=1000, height=450)
plotnet(nn.iris)
dev.off()

predictions <-  predict(nn.iris, iris.test, type = "class")

table(iris.test$Species, predictions)

olden(nn.iris) # Variable importance is based on connection weights. 

################################################################################
# Project 2: Train an algorithm to predict whether the currency is fake or not
################################################################################

setwd("C:/Users/ardin/OneDrive/Desktop/Econ 258 R Fall 2020/RCodes")

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

# Step 2: Create a training and test set. 

set.seed(810)

train      <- sample(1:nrow(banknote), nrow(banknote)/2)
notes.train <- banknote[train,]
notes.test  <- banknote[-train,]


# Step 3: Create a neural network to predict legit (1) versus fake (0) currency with 3 hidden layers. 
#         Summarize the model and draw the graph. 

nn.banknote <- nnet(class ~ ., notes.train, size=3)

summary(nn.banknote)

png("graph_neural_network2.png", width=1000, height=450)
plotnet(nn.banknote)
dev.off()

# Step 4: Use the model to predict information in the test data. Since its a classification, do not forget to specify type="class". 
#         Then plot the model. 

predictions <-  predict(nn.banknote, notes.test, type = "class")

# Step 5: Evaluate your model. 
#         Create a table showing actual versus predicted class variable.
#         What percentage are you able to predict correctly?

table(notes.test$class, predictions)

# Step 6: Graph variable importance graph. Which variable is most important? 

olden(nn.banknote) # Variable importance is based on connection weights. 
