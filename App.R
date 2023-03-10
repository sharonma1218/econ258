
#----------------------------------
# Title: Econ 258 Shiny Project 
# Authors: Sharon, Oleg, Ibnu 
#----------------------------------

library(shinyWidgets)
library(shinythemes)
library(mosaic)
library(haven)
library(dplyr)
library(stats)
library(ggplot2)
library(stargazer)
library(nnet)
library(NeuralNetTools)
library(randomForest)
library(glmnet)
library(tidyverse)
library(stringr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(MASS)
library(randomForest)

# setwd("/Users/sharonma0975/Downloads/Econ 258 R/Shiny Project")

#-----------
# Load Data
#-----------

load(file='34792-0001-Data.rda')
working_data <- da34792.0001
working_data <- dplyr::select(working_data, c("CASEID","SCHL_ID","DIST_ID","Q1","Q7","Q8","Q38A",
                                              "Q38B","Q38C","Q38D","Q38E","Q38F","Q42D","Q42E",
                                              "Q42F","Q42G","Q43C","Q43D","Q48A","Q48B","Q48C",
                                              "Q48D","Q9_1","Q9_2","Q9_COMP","Q10_1","Q10_2",
                                              "Q10_COMP","Q19","Q61","Q1","Q3B","Q4","Q5",
                                              "Q6_1","Q6_2","Q6_3","Q6_4","Q6_5","Q6_COMP", "Q61"))

#------------
# Clean Data
#------------

# Drop NAs 
working_data <- filter(working_data, is.na(DIST_ID)==FALSE)
working_data <- filter(working_data, is.na(Q7)==FALSE)
working_data <- filter(working_data, is.na(Q8)==FALSE)
working_data <- filter(working_data, is.na(Q38A)==FALSE)
working_data <- filter(working_data, is.na(Q38B)==FALSE)
working_data <- filter(working_data, is.na(Q38C)==FALSE)
working_data <- filter(working_data, is.na(Q38D)==FALSE)
working_data <- filter(working_data, is.na(Q38E)==FALSE)
working_data <- filter(working_data, is.na(Q38F)==FALSE)
working_data <- filter(working_data, is.na(Q42D)==FALSE)
working_data <- filter(working_data, is.na(Q42E)==FALSE)
working_data <- filter(working_data, is.na(Q42F)==FALSE)
working_data <- filter(working_data, is.na(Q42G)==FALSE)
working_data <- filter(working_data, is.na(Q43C)==FALSE)
working_data <- filter(working_data, is.na(Q43D)==FALSE)
working_data <- filter(working_data, is.na(Q48A)==FALSE)
working_data <- filter(working_data, is.na(Q48B)==FALSE)
working_data <- filter(working_data, is.na(Q48C)==FALSE)
working_data <- filter(working_data, is.na(Q48D)==FALSE)
working_data <- filter(working_data, is.na(Q9_1)==FALSE)
working_data <- filter(working_data, is.na(Q9_2)==FALSE)
working_data <- filter(working_data, is.na(Q9_COMP)==FALSE)
working_data <- filter(working_data, is.na(Q10_1)==FALSE)
working_data <- filter(working_data, is.na(Q10_2)==FALSE)
working_data <- filter(working_data, is.na(Q10_COMP)==FALSE)
working_data <- filter(working_data, is.na(Q19)==FALSE)
working_data <- filter(working_data, is.na(Q61)==FALSE)
working_data <- filter(working_data, is.na(Q1)==FALSE)
working_data <- filter(working_data, is.na(Q3B)==FALSE)
working_data <- filter(working_data, is.na(Q5)==FALSE)
working_data <- filter(working_data, is.na(Q6_COMP)==FALSE)

#-----------------------------------------------
# Reg all potentially relevant variables on Q7
#-----------------------------------------------

reg01 <- lm(Q7~Q8+Q38A+Q38B+Q38C+Q38D+Q38E+Q38F+Q42D+Q42E+Q42F+Q42G+Q43C+Q43D+Q48A+
              Q48B+Q48C+Q48D+Q9_1+Q9_2+Q9_COMP+Q10_1+Q10_2+Q10_COMP+Q19+Q61, 
            data=working_data)
summary(reg01)

#--------------------
# Quantify Variables
#--------------------

dplyr::rename(working_data, gender = Q1)

working_data$q8new <- 0 
working_data$q8new[working_data$Q8=="(1) Much too thin"] <- 1
working_data$q8new[working_data$Q8=="(2) A bit too thin"] <- 2
working_data$q8new[working_data$Q8=="(3) About the right size"] <- 3
working_data$q8new[working_data$Q8=="(4) A bit too fat"] <- 4
working_data$q8new[working_data$Q8=="(5) Much too fat"] <- 5
table(working_data$Q8) # Checks 
table(working_data$q8new)

working_data$q38anew <- 0 
working_data$q38anew[working_data$Q38A=="(1) Strongly disagree"] <- 1 
working_data$q38anew[working_data$Q38A=="(2) Disagree"] <- 2
working_data$q38anew[working_data$Q38A=="(3) Neither agree or disagree"] <- 3
working_data$q38anew[working_data$Q38A=="(4) Agree"] <- 4
working_data$q38anew[working_data$Q38A=="(5) Strongly agree"] <- 5
table(working_data$Q38A) # Checks 
table(working_data$q38anew)

working_data$q38bnew <- 0
working_data$q38bnew[working_data$Q38B=="(1) Strongly disagree"] <- 1 
working_data$q38bnew[working_data$Q38B=="(2) Disagree"] <- 2
working_data$q38bnew[working_data$Q38B=="(3) Neither agree or disagree"] <- 3
working_data$q38bnew[working_data$Q38B=="(4) Agree"] <- 4
working_data$q38bnew[working_data$Q38B=="(5) Strongly agree"] <- 5
table(working_data$Q38B) # Checks 
table(working_data$q38bnew)

working_data$q38cnew <- 0
working_data$q38cnew[working_data$Q38C=="(1) Strongly disagree"] <- 1 
working_data$q38cnew[working_data$Q38C=="(2) Disagree"] <- 2
working_data$q38cnew[working_data$Q38C=="(3) Neither agree or disagree"] <- 3
working_data$q38cnew[working_data$Q38C=="(4) Agree"] <- 4
working_data$q38cnew[working_data$Q38C=="(5) Strongly agree"] <- 5
table(working_data$Q38C) # Checks 
table(working_data$q38cnew)

working_data$q38dnew <- 0
working_data$q38dnew[working_data$Q38D=="(1) Strongly disagree"] <- 1 
working_data$q38dnew[working_data$Q38D=="(2) Disagree"] <- 2
working_data$q38dnew[working_data$Q38D=="(3) Neither agree or disagree"] <- 3
working_data$q38dnew[working_data$Q38D=="(4) Agree"] <- 4
working_data$q38dnew[working_data$Q38D=="(5) Strongly agree"] <- 5
table(working_data$Q38D) # Checks 
table(working_data$q38dnew)

working_data$q38enew <- 0
working_data$q38enew[working_data$Q38E=="(1) Strongly disagree"] <- 1 
working_data$q38enew[working_data$Q38E=="(2) Disagree"] <- 2
working_data$q38enew[working_data$Q38E=="(3) Neither agree or disagree"] <- 3
working_data$q38enew[working_data$Q38E=="(4) Agree"] <- 4
working_data$q38enew[working_data$Q38E=="(5) Strongly agree"] <- 5
table(working_data$Q38E) # Checks 
table(working_data$q38enew)

working_data$q38fnew <- 0
working_data$q38fnew[working_data$Q38F=="(1) Strongly disagree"] <- 1 
working_data$q38fnew[working_data$Q38F=="(2) Disagree"] <- 2
working_data$q38fnew[working_data$Q38F=="(3) Neither agree or disagree"] <- 3
working_data$q38fnew[working_data$Q38F=="(4) Agree"] <- 4
working_data$q38fnew[working_data$Q38F=="(5) Strongly agree"] <- 5
table(working_data$Q38F) # Checks 
table(working_data$q38fnew)

working_data$q42dnew <- 0
working_data$q42dnew[working_data$Q42D=="(1) About every day"] <- 1 
working_data$q42dnew[working_data$Q42D=="(2) More than once a week"] <- 2
working_data$q42dnew[working_data$Q42D=="(3) About every week"] <- 3
working_data$q42dnew[working_data$Q42D=="(4) About every month"] <- 4
working_data$q42dnew[working_data$Q42D=="(5) Rarely or never"] <- 5
table(working_data$Q42D) # Checks 
table(working_data$q42dnew)

working_data$q42enew <- 0
working_data$q42enew[working_data$Q42E=="(1) About every day"] <- 1 
working_data$q42enew[working_data$Q42E=="(2) More than once a week"] <- 2
working_data$q42enew[working_data$Q42E=="(3) About every week"] <- 3
working_data$q42enew[working_data$Q42E=="(4) About every month"] <- 4
working_data$q42enew[working_data$Q42E=="(5) Rarely or never"] <- 5
table(working_data$Q42E) # Checks 
table(working_data$q42enew)

working_data$q42fnew <- 0
working_data$q42fnew[working_data$Q42F=="(1) About every day"] <- 1 
working_data$q42fnew[working_data$Q42F=="(2) More than once a week"] <- 2
working_data$q42fnew[working_data$Q42F=="(3) About every week"] <- 3
working_data$q42fnew[working_data$Q42F=="(4) About every month"] <- 4
working_data$q42fnew[working_data$Q42F=="(5) Rarely or never"] <- 5
table(working_data$Q42F) # Checks 
table(working_data$q42fnew)

working_data$q42gnew <- 0
working_data$q42gnew[working_data$Q42G=="(1) About every day"] <- 1 
working_data$q42gnew[working_data$Q42G=="(2) More than once a week"] <- 2
working_data$q42gnew[working_data$Q42G=="(3) About every week"] <- 3
working_data$q42gnew[working_data$Q42G=="(4) About every month"] <- 4
working_data$q42gnew[working_data$Q42G=="(5) Rarely or never"] <- 5
table(working_data$Q42G) # Checks 
table(working_data$q42gnew)

working_data$q43cnew <- 0
working_data$q43cnew[working_data$Q43C=="(1) No"] <- 1
working_data$q43cnew[working_data$Q43C=="(2) Yes, once"] <- 2
working_data$q43cnew[working_data$Q43C=="(3) Yes, more than once"] <- 3
table(working_data$Q43C) # Checks 
table(working_data$q43cnew)

working_data$q43dnew <- 0
working_data$q43dnew[working_data$Q43D=="(1) No"] <- 1
working_data$q43dnew[working_data$Q43D=="(2) Yes, once"] <- 2
working_data$q43dnew[working_data$Q43D=="(3) Yes, more than once"] <- 3
table(working_data$Q43D) # Checks 
table(working_data$q43dnew)

working_data$q48anew <- 0
working_data$q48anew[working_data$Q48A=="(1) Never"] <- 1
working_data$q48anew[working_data$Q48A=="(2) Seldom"] <- 2
working_data$q48anew[working_data$Q48A=="(3) Quite often"] <- 3
working_data$q48anew[working_data$Q48A=="(4) Very often"] <- 4
working_data$q48anew[working_data$Q48A=="(5) Always"] <- 5
table(working_data$Q48A) # Checks 
table(working_data$q48anew)

working_data$q48bnew <- 0
working_data$q48bnew[working_data$Q48B=="(1) Never"] <- 1
working_data$q48bnew[working_data$Q48B=="(2) Seldom"] <- 2
working_data$q48bnew[working_data$Q48B=="(3) Quite often"] <- 3
working_data$q48bnew[working_data$Q48B=="(4) Very often"] <- 4
working_data$q48bnew[working_data$Q48B=="(5) Always"] <- 5
table(working_data$Q48B) # Checks 
table(working_data$q48bnew)

working_data$q48cnew <- 0
working_data$q48cnew[working_data$Q48C=="(1) Never"] <- 1
working_data$q48cnew[working_data$Q48C=="(2) Seldom"] <- 2
working_data$q48cnew[working_data$Q48C=="(3) Quite often"] <- 3
working_data$q48cnew[working_data$Q48C=="(4) Very often"] <- 4
working_data$q48cnew[working_data$Q48C=="(5) Always"] <- 5
table(working_data$Q48C) # Checks 
table(working_data$q48cnew)

working_data$q48dnew <- 0
working_data$q48dnew[working_data$Q48D=="(1) Never"] <- 1
working_data$q48dnew[working_data$Q48D=="(2) Seldom"] <- 2
working_data$q48dnew[working_data$Q48D=="(3) Quite often"] <- 3
working_data$q48dnew[working_data$Q48D=="(4) Very often"] <- 4
working_data$q48dnew[working_data$Q48D=="(5) Always"] <- 5
table(working_data$Q48D) # Checks 
table(working_data$q48dnew)

working_data$q9_1new <- 0
working_data$q9_1new[working_data$Q9_1=="(1) None at all"] <- 1 
working_data$q9_1new[working_data$Q9_1=="(2) About half an hour a day"] <- 2
working_data$q9_1new[working_data$Q9_1=="(3) About 1 hour a day"] <- 3
working_data$q9_1new[working_data$Q9_1=="(4) About 2 hours a day"] <- 4
working_data$q9_1new[working_data$Q9_1=="(5) About 3 hours a day"] <- 5
working_data$q9_1new[working_data$Q9_1=="(6) About 4 hours a day"] <- 6
working_data$q9_1new[working_data$Q9_1=="(7) About 5 hours a day"] <- 7
working_data$q9_1new[working_data$Q9_1=="(8) About 6 hours a day"] <- 8
working_data$q9_1new[working_data$Q9_1=="(9) About 7 hours or more a day"] <- 9
table(working_data$Q9_1) # Checks 
table(working_data$q9_1new)

working_data$q9_2new <- 0
working_data$q9_2new[working_data$Q9_2=="(1) None at all"] <- 1 
working_data$q9_2new[working_data$Q9_2=="(2) About half an hour a day"] <- 2
working_data$q9_2new[working_data$Q9_2=="(3) About 1 hour a day"] <- 3
working_data$q9_2new[working_data$Q9_2=="(4) About 2 hours a day"] <- 4
working_data$q9_2new[working_data$Q9_2=="(5) About 3 hours a day"] <- 5
working_data$q9_2new[working_data$Q9_2=="(6) About 4 hours a day"] <- 6
working_data$q9_2new[working_data$Q9_2=="(7) About 5 hours a day"] <- 7
working_data$q9_2new[working_data$Q9_2=="(8) About 6 hours a day"] <- 8
working_data$q9_2new[working_data$Q9_2=="(9) About 7 hours or more a day"] <- 9
table(working_data$Q9_2) # Checks 
table(working_data$q9_2new)

working_data$q10_1new <- 0
working_data$q10_1new[working_data$Q10_1=="(1) None at all"] <- 1 
working_data$q10_1new[working_data$Q10_1=="(2) About half an an hour a day"] <- 2
working_data$q10_1new[working_data$Q10_1=="(3) About 1 hour a day"] <- 3
working_data$q10_1new[working_data$Q10_1=="(4) About 2 hours a day"] <- 4
working_data$q10_1new[working_data$Q10_1=="(5) About 3 hours a day"] <- 5
working_data$q10_1new[working_data$Q10_1=="(6) About 4 hours a day"] <- 6
working_data$q10_1new[working_data$Q10_1=="(7) About 5 hours a day"] <- 7
working_data$q10_1new[working_data$Q10_1=="(8) About 6 hours a day"] <- 8
working_data$q10_1new[working_data$Q10_1=="(9) About 7 or more hours a day"] <- 9
table(working_data$Q10_1) # Checks 
table(working_data$q10_1new)

working_data$q10_2new <- 0
working_data$q10_2new[working_data$Q10_2=="(1) None at all"] <- 1 
working_data$q10_2new[working_data$Q10_2=="(2) About half an an hour a day"] <- 2
working_data$q10_2new[working_data$Q10_2=="(3) About 1 hour a day"] <- 3
working_data$q10_2new[working_data$Q10_2=="(4) About 2 hours a day"] <- 4
working_data$q10_2new[working_data$Q10_2=="(5) About 3 hours a day"] <- 5
working_data$q10_2new[working_data$Q10_2=="(6) About 4 hours a day"] <- 6
working_data$q10_2new[working_data$Q10_2=="(7) About 5 hours a day"] <- 7
working_data$q10_2new[working_data$Q10_2=="(8) About 6 hours a day"] <- 8
working_data$q10_2new[working_data$Q10_2=="(9) About 7 or more hours a day"] <- 9
table(working_data$Q10_2) # Checks 
table(working_data$q10_2new)

working_data$q9_comp_new <- 0
working_data$q9_comp_new[working_data$Q9_COMP=="(1) None at all"] <- 1 
working_data$q9_comp_new[working_data$Q9_COMP=="(2) Less Than 1 Hour"] <- 2
working_data$q9_comp_new[working_data$Q9_COMP=="(3) 1 to 1.99 Hours"] <- 3
working_data$q9_comp_new[working_data$Q9_COMP=="(4) 2 to 2.99 Hours"] <- 4
working_data$q9_comp_new[working_data$Q9_COMP=="(5) 3 to 3.99 Hours"] <- 5
working_data$q9_comp_new[working_data$Q9_COMP=="(6) 4 to 4.99 Hours"] <- 6
working_data$q9_comp_new[working_data$Q9_COMP=="(7) 5 to 5.99 Hours"] <- 7
working_data$q9_comp_new[working_data$Q9_COMP=="(8) 6 to 6.99 Hours"] <- 8
working_data$q9_comp_new[working_data$Q9_COMP=="(9) 7 or More Hours"] <- 9
table(working_data$Q9_COMP) # Checks 
table(working_data$q9_comp_new)

working_data$q10_comp_new <- 0
working_data$q10_comp_new[working_data$Q10_COMP=="(1) None at all"] <- 1 
working_data$q10_comp_new[working_data$Q10_COMP=="(2) Less Than 1 Hour"] <- 2
working_data$q10_comp_new[working_data$Q10_COMP=="(3) 1 to 1.99 Hours"] <- 3
working_data$q10_comp_new[working_data$Q10_COMP=="(4) 2 to 2.99 Hours"] <- 4
working_data$q10_comp_new[working_data$Q10_COMP=="(5) 3 to 3.99 Hours"] <- 5
working_data$q10_comp_new[working_data$Q10_COMP=="(6) 4 to 4.99 Hours"] <- 6
working_data$q10_comp_new[working_data$Q10_COMP=="(7) 5 to 5.99 Hours"] <- 7
working_data$q10_comp_new[working_data$Q10_COMP=="(8) 6 to 6.99 Hours"] <- 8
working_data$q10_comp_new[working_data$Q10_COMP=="(9) 7 or More Hours"] <- 9
table(working_data$Q10_COMP) # Checks 
table(working_data$q10_comp_new)

working_data$q19new <- 0
working_data$q19new[working_data$Q19=="(0) 0 days"] <- 0 
working_data$q19new[working_data$Q19=="(1) 1 day"] <- 1
working_data$q19new[working_data$Q19=="(2) 2 days"] <- 2
working_data$q19new[working_data$Q19=="(3) 3 days"] <- 3
working_data$q19new[working_data$Q19=="(4) 4 days"] <- 4
working_data$q19new[working_data$Q19=="(5) 5 days"] <- 5
working_data$q19new[working_data$Q19=="(6) 6 days"] <- 6
working_data$q19new[working_data$Q19=="(7) 7 days"] <- 7
table(working_data$Q19) # Checks 
table(working_data$q19new)

working_data$q61new <- 0 
working_data$q61new[working_data$Q61=="(1) Very good"] <- 1
working_data$q61new[working_data$Q61=="(2) Good"] <- 2
working_data$q61new[working_data$Q61=="(3) Average"] <- 3
working_data$q61new[working_data$Q61=="(4) Below average"] <- 4
table(working_data$Q61) # Checks 
table(working_data$q61new)

# rename races 
working_data$q6new[working_data$Q6_COMP=="(1) Black or African American"] <- "Black or African American"
working_data$q6new[working_data$Q6_COMP=="(2) White"] <- "White"
working_data$q6new[working_data$Q6_COMP=="(3) Asian"] <- "Asian"
working_data$q6new[working_data$Q6_COMP=="(4) American Indian or Alaska Native"] <- "American Indian or Alaska Native"
working_data$q6new[working_data$Q6_COMP=="(5) Native Hawaiian or Other Pacific Islander"] <- "Native Hawaiian or Other Pacific Islander"
working_data$q6new[working_data$Q6_COMP=="(7) Hispanic"] <- "Hispanic" 
working_data$q6new[working_data$Q6_COMP=="(6) Two or More Races"] <- "Two or More Races"
table(working_data$Q6_COMP) # Checks 
table(working_data$q6new)

#------------
# Lasso Reg
#------------

# load data
y <- working_data$Q7
x <- data.matrix(working_data[,c('Q8','Q38A','Q38B','Q38C','Q38D','Q38E','Q38F',
                                 'Q42D','Q42E','Q42F','Q42G','Q43C','Q43D','Q48A',
                                 'Q48B','Q48C','Q48D','Q9_1','Q9_2','Q9_COMP',
                                 'Q10_1','Q10_2','Q10_COMP','Q19','Q61')])

# perform k-fold cross-validation to find optimal lambda value 
cv_model <- cv.glmnet(x,y,alpha=1)

# find optimal lambda value that minimizes test MSE 
best_lambda <- cv_model$lambda.min
best_lambda # 0.002503213

# produce plot of test MSE by lambda value
lasso_graph <- plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x,y,alpha=1,lambda=best_lambda)
coef(best_model)

# this exercise confirms that the statistically significant variables are relevant 










#------------
# Machine Learning
#------------

#--------
# Rename
#--------

names(working_data)[names(working_data)=="q8new"] <- "q8"
names(working_data)[names(working_data)=="q38anew"] <- "q38a"
names(working_data)[names(working_data)=="q38bnew"] <- "q38b"
names(working_data)[names(working_data)=="q38bnew"] <- "q38b"
names(working_data)[names(working_data)=="q38cnew"] <- "q38c"
names(working_data)[names(working_data)=="q38dnew"] <- "q38d"
names(working_data)[names(working_data)=="q38enew"] <- "q38e"
names(working_data)[names(working_data)=="q38fnew"] <- "q38f"
names(working_data)[names(working_data)=="q42dnew"] <- "q42d"
names(working_data)[names(working_data)=="q42enew"] <- "q42e"
names(working_data)[names(working_data)=="q42fnew"] <- "q42f"
names(working_data)[names(working_data)=="q42gnew"] <- "q42g"
names(working_data)[names(working_data)=="q43cnew"] <- "q43c"
names(working_data)[names(working_data)=="q43dnew"] <- "q43d"
names(working_data)[names(working_data)=="q48anew"] <- "q48a"
names(working_data)[names(working_data)=="q48bnew"] <- "q48b"
names(working_data)[names(working_data)=="q48cnew"] <- "q48c"
names(working_data)[names(working_data)=="q48dnew"] <- "q48d"
names(working_data)[names(working_data)=="q9_1new"] <- "q9_1"
names(working_data)[names(working_data)=="q9_2new"] <- "q9_2"
names(working_data)[names(working_data)=="q9_comp_new"] <- "q9_comp"
names(working_data)[names(working_data)=="q10_1new"] <- "q10_1"
names(working_data)[names(working_data)=="q10_2new"] <- "q10_2"
names(working_data)[names(working_data)=="q10_comp_new"] <- "q10_comp"
names(working_data)[names(working_data)=="q19new"] <- "q19"
names(working_data)[names(working_data)=="q61new"] <- "q61"

# define frame that only contains relevant variables (mostly for random forest's sake)

# THIS NEEDS FIXING AS POTENTIAL EXPLANATORY VARIABLES CHANGE 
Q7_and_explanatory <- dplyr::select(working_data, c("Q7", "q8", "q38a","q38b","q38c","q38d","q38e","q38f", "q42d", "q42e","q42f", "q42g","q43c","q43d", "q48a",
                                                    "q48b", "q48c", "q48d","q9_1","q9_2", "q9_comp", "q10_1","q10_2", "q10_comp", "q19", "q61"))

# POINT OF REFERENCE: LIFE SATISFACTION SCALE RESPONSES GO 0-10

# seed chosen arbitrarily
set.seed(367)

train <- sample(1:nrow(working_data), 100)
working.train <- working_data[train,]
working.test <- working_data[-train,]

train <- sample(1:nrow(Q7_and_explanatory), 100)
Q7_exp.train <- Q7_and_explanatory[train,]
Q7_exp.test <- Q7_and_explanatory[-train,]

# Goal: determine which variables associated with mental health are best predictors of life satisfaction score (Q7)

# Neural Network

neural1 <- nnet(Q7 ~ q8 + q38a + q38b + q38c + q38d + q38e + q38f + q42d + q42e + q42f + q42g + q43c + q43d + q48a + 
                  q48b + q48c + q48d + q9_1 + q9_2 + q9_comp + q10_1 + q10_2 + q10_comp + q19 + q61, Q7_and_explanatory, size=4)

neural2 <- nnet(Q7 ~ q8 + q38a + q38b + q38c + q38d + q38e + q38f + q42d + q42e + q42f + q42g + q43c + q43d + q48a + 
                  q48b + q48c + q48d + q9_1 + q9_2 + q9_comp + q10_1 + q10_2 + q10_comp + q19 + q61, working_data, size=4)

predictions1 <- predict(neural1, Q7_exp.test, type = "raw")      
predictions2 <- predict(neural2, working.test, type = "raw") 

table(working.test$Q7, predictions1)
table(working.test$Q7, predictions2)

nn_plot <- olden(neural1) 
olden(neural2)

dev.off()

# seed chosen arbitrarily
set.seed(367)

# Bagging
bagging=randomForest(Q7~.,data=Q7_and_explanatory,subset=train,mtry=4,importance=TRUE)

importance(bagging)
varImpPlot(bagging)
# seed chosen arbitrarily
set.seed(367)

# Random Forest

rndForest=randomForest(Q7~.,data=Q7_and_explanatory,subset=train,mtry=13,importance=TRUE)

importance(rndForest)
varImpPlot(rndForest)

# Look at partial dependence plots.
Q7_exp_train <- Q7_and_explanatory[train,]
partialPlot(rndForest, pred.data = Q7_exp_train, x.var = "q42d") 
partialPlot(bagging, pred.data = Q7_exp_train, x.var = "q48b")

dev.off()

#-----------------------------------------------------
# Reg all statistically significant variables on Q7
#-----------------------------------------------------

reg02 <- lm(Q7~q8+q38a+q38b+q38c+q38d+q38e+q38f+q42d+
              q42e+q42f+q42g+q43c+q43d+q48a+q48b+q48c+
              q48d+q9_1+q9_2+q9_comp+q10_1+q10_2+q10_comp+
              q19+q61, data=working_data)
summary(reg02)

#-------------------------------
# Subset of Good Mental Health
#-------------------------------

good_mh <- filter(working_data, (q8==3 & q38b>=4 & q42d>=4 & q42g>=4 &
                                   q48a>=4 & q48b>=4 | q48c<=2 | q48d<=2))

#-------
# Stats
#-------

# students w/ good mental health & teacher's opinion on their performance 

table(good_mh$q61new) # the typical student w/ good mental health usually has "very good" or "good" performances in school 

teacher_graph <- ggplot(working_data, aes(x=Q61)) +
  geom_bar(fill="dodgerblue4") +
  labs(x="Teacher's Opinion on Their Performance",
       y="Frequency",
       title="Students with Good Mental Health") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5))

print(teacher_graph)

# proportion life satisfaction & gender 
bar_plot <- working_data %>% dplyr::select(Q7, Q1)
gender_graph <- ggplot(bar_plot, 
                       aes(x = Q7, 
                           fill = Q1)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction Between Males and Females", y = "Proportion", x = "Life Satisfaction Score", fill='Gender') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & physically active 
bar_plot_1 <- working_data %>% dplyr::select(Q7, Q1, Q19)
phys_active <- ggplot(bar_plot_1, 
                      aes(x = Q7, 
                          fill = Q19)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Physically Active in Days", y = "Proportion", x = "Life Satisfaction Scale", fill='Days Physically Active') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & daily computer use
bar_plot_2 <- working_data %>% dplyr::select(Q7, Q1, Q10_COMP)
comp_use <- ggplot(bar_plot_2, 
                   aes(x = Q7, 
                       fill = Q10_COMP)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Daily Computer Use", y = "Proportion", x = "Life Satisfaction Scale", fill='Hours of Daily Computer Use') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & race

working_data$q6new[working_data$Q6_COMP=="(1) Black or African American"] <- "Black or African American"
working_data$q6new[working_data$Q6_COMP=="(2) White"] <- "White"
working_data$q6new[working_data$Q6_COMP=="(3) Asian"] <- "Asian"
working_data$q6new[working_data$Q6_COMP=="(4) American Indian or Alaska Native"] <- "American Indian or Alaska Native"
working_data$q6new[working_data$Q6_COMP=="(5) Native Hawaiian or Other Pacific Islander"] <- "Native Hawaiian or Other Pacific Islander"
working_data$q6new[working_data$Q6_COMP=="(7) Hispanic"] <- "Hispanic" 
working_data$q6new[working_data$Q6_COMP=="(6) Two or More Races"] <- "Two or More Races"
working_data <- filter(working_data, is.na(q6new)==FALSE)
table(working_data$Q6_COMP) # Checks 
table(working_data$q6new)

bar_plot_3 <- working_data %>% dplyr::select(Q7, Q1, q6new)

race_graph <- ggplot(bar_plot_3, 
                     aes(x = Q7, 
                         fill = q6new)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Race", y = "Proportion", x = "Life Satisfaction Score", fill='Race') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & body image
bar_plot_4 <- working_data %>% dplyr::select(Q7, Q1, Q38C)
body_graph <- ggplot(bar_plot_4, 
                     aes(x = Q7, 
                         fill = Q38C)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Negative Body Image", y = "Proportion", x = "Life Satisfaction Score", fill='Do you hate your body?') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & appearance satisfaction 
bar_plot_5 <- working_data %>% dplyr::select(Q7, Q1, Q38B)
appear_graph <- ggplot(bar_plot_5, 
                       aes(x = Q7, 
                           fill = Q38B)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Appearance Satisfaction", y = "Proportion", x = "Life Satisfaction Score", fill='Are you satified with your appereance?') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & difficulty sleeping 
bar_plot_6 <- working_data %>% dplyr::select(Q7, Q1, Q42G)
sleep_graph <- ggplot(bar_plot_6, 
                      aes(x = Q7, 
                          fill = Q42G)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Difficulties of Sleeping", y = "Proportion", x = "Life Satisfaction Score", fill='how often have you had sleeping difficulties?') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# proportion life satisfaction & loneliness
bar_plot_7 <- working_data %>% dplyr::select(Q7, Q1, Q48D)
lonely_graph <- ggplot(bar_plot_7, 
                       aes(x = Q7, 
                           fill = Q48D)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and lonelinesss", y = "Proportion", x = "Life Satisfaction Scale", fill='Have you felt lonely?') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# proportion life satisfaction & sadness
bar_plot_8 <- working_data %>% dplyr::select(Q7, Q1, Q48C)
sad_graph <- ggplot(bar_plot_8, 
                    aes(x = Q7, 
                        fill = Q48C)) + 
  geom_bar(position = "fill") +
  labs(title = "Life Satisfaction and Sadness", y = "Proportion", x = "Life Satisfaction Scale", fill='Have you felt sad?') + scale_fill_brewer() + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )





ui <- navbarPage("Mental Health Trends Among Students",
                 
                 
                 # HOME TAB
                 tabPanel("Home", fluidPage( theme = shinytheme("cerulean"),
                                             
                                             # DEFINE COLOR FOR THE APP HERE
                                             setBackgroundColor(
                                               color = c("white"),
                                               gradient = "linear",
                                               direction = "bottom"),
                                             
                                             h1("Mental Health Trends Among Middle School and High School Students"),
                                             h3("Authors:  Oleg Khaytarov, Ibnu Prayogo, Sharon Ma"),
                                             hr(),
                                             h2("Welcome to our Shiny App!"),
                                             h3("Motivation:"),
                                             br(),
                                             textOutput("home1"),
                                             br(),
                                             h3("Data Source"),
                                             br(),
                                             textOutput("dataset1"),
                                             br(),
                                             h3("Description Of Each Tab:"),
                                             br(),
                                             textOutput("tab1"),
                                             br(),
                                             textOutput("tab2"),
                                             br(),
                                             textOutput("tab3"),
                                             br(),
                                             textOutput("tab4"),
                                             br(),
                                             textOutput("tab5"),
                                             br(),
                                             textOutput("tab6"),
                                             br(),
                                             br()
                 )),
                 
                 # Machine Learning Legend
                 tabPanel("Machine Learning, Legend", fluidPage(
                   h1("Guide to understanding the machine learning algorithms utilized in this project"),
                   hr(),
                   h2("What is the goal of utilizing machine learning in this study?"),
                   br(),
                   textOutput("ML_purpose"),
                   br(),
                   h2("What is each machine learning method, exactly?"),
                   br(),
                   textOutput("ML_methods_nn"),
                   br(),
                   textOutput("ML_methods_bag"),
                   br(),
                   textOutput("ML_methods_rf"),
                   br(),
                   h2("How to read the graphs?"),
                   br(),
                   textOutput("how_to_nn"),
                   br(),
                   textOutput("how_to_bag_and_rf"),
                   br(),
                   br(),
                   h2("Variables:"),
                   br(),
                   h4("The variable names in the machine learning are non-descriptive, here's the legend for understanding what each codename stand for:"),
                   br(),
                   br(),
                   textOutput("legend1"),
                   br(),
                   br(),
                   textOutput("legend2"),
                   br(),
                   textOutput("legend3"),
                   br(),
                   br(),
                   textOutput("extra"),
                   br(),
                   textOutput("age"),
                   br(),
                   textOutput("gender"),
                   br(),
                   br(),
                   textOutput("legend4"),
                   br(),
                   textOutput("legend5"),
                   br(),
                   br(),
                   textOutput("legend6"),
                   br(),
                   textOutput("legend7"),
                   br(),
                   textOutput("legend8"),
                   br(),
                   textOutput("legend9"),
                   br(),
                   textOutput("legend10"),
                   br(),
                   textOutput("legend11"),
                   br(),
                   br(),
                   textOutput("legend12"),
                   br(),
                   textOutput("legend13"),
                   br(),
                   textOutput("legend14"),
                   br(),
                   textOutput("legend15"),
                   br(),
                   textOutput("legend16"),
                   br(),
                   textOutput("legend17"),
                   br(),
                   br(),
                   textOutput("legend18"),
                   br(),
                   textOutput("legend19"),
                   br(),
                   textOutput("legend20"),
                   br(),
                   textOutput("legend21"),
                   br(),
                   textOutput("legend22"),
                   br(),
                   br(),
                   textOutput("legend23"),
                   br(),
                   textOutput("legend24"),
                   br(),
                   textOutput("legend25"),
                   br(),
                   br(),
                   textOutput("legend26"),
                   br(),
                   textOutput("legend27"),
                   br(),
                   textOutput("legend28"),
                   br(),
                   textOutput("legend29"),
                   br(),
                   textOutput("legend30"),
                   br(),
                   br(),
                   textOutput("legend31"),
                   br(),
                   textOutput("legend32"),
                   br(),
                   textOutput("legend33"),
                   br(),
                   textOutput("legend34"),
                   br(),
                   textOutput("legend35"),
                   br(),
                   textOutput("legend36"),
                   br(),
                   textOutput("legend37"),
                   br(),
                   br(),
                   textOutput("legend38"),
                   br(),
                   textOutput("legend39"),
                   br(),
                   textOutput("legend40"),
                   br(),
                   br()
                 )),
                 
                 # Machine Learning
                 tabPanel("Machine Learning, Output Graphs", fluidPage(
                   h1("Machine Learning Algorithms"),
                   hr(),
                   br(),
                   br(),
                   radioButtons(inputId = "ML_input", label = "Machine Learning", choices = c("Neural Network","Bagging", "Random Forest")),
                   br(),
                   br(),
                   plotOutput("MLPlot")),
                   br(),
                   br()),
                   
                 # Demographics
                 tabPanel("Demographics", fluidPage(
                   h1("What is the relationship between mental health and certain demographics?"),
                   hr("i.e. Is there a specific gender or race that tends to have a higher life satisfaction score than others?"),
                   radioButtons(inputId = "Dem_input", label = "", choices = c("Gender","Race")),
                   plotOutput("DemPlot")),
                   br("Data Source: 'Health Behavior in School-Aged Children, 2009-2010' by Ronald J. Iannotti"),
                   br("As you can see, males to be more extreme in how they perceive life, compared to their female counterparts. 
                      i.e. They are more likely to either be extremely disastified or satisified with their life than females. 
                      In addition, whites tend to have a higher life satisfaction score than others. 
                      However, we can only determine correlation and not causation. 
                      i.e. We cannot determine that being male or white results in higher life satisfaction scores.
                      Also, the fact that there are simply more whites than any other race in our data could have skewed our results"),
                   br(),
                   br()),
                 
                 # Self-Perception
                 tabPanel("Self-Perception", fluidPage(
                   h1("What is the relationship between mental health and self-perception?"),
                   hr("i.e. Is there a relationship between appearance satisfaction or negative body image and life satisfaction scores?"),
                   radioButtons(inputId = "Self_input", label = "", choices = c("Appearance Satisfaction", "Negative Body Image")),
                   plotOutput("SelfPlot")),
                   br("Data Source: 'Health Behavior in School-Aged Children, 2009-2010' by Ronald J. Iannotti"),
                   br("As you can see, there is a positive correlation between appearance satisfaction and life satisfaction:
                      the more satisfied the student is with their appearance, the more satisfied they are with their life.
                      There is also a negative correlation between negative body image and life satisfaction:
                      the more they hate their body, the less satisfied they are with their life."),
                   br(),
                   br()),
                 
                 # Emotions
                 tabPanel("Emotions", fluidPage(
                   h1("What is the relationship between mental health and certain emotions?"),
                   hr("Select one of the following emotions:"),
                   radioButtons(inputId = "Emo_input", label = "", choices = c("Loneliness", "Sadness")),
                   plotOutput("EmoPlot")),
                   br("Data Source: 'Health Behavior in School-Aged Children, 2009-2010' by Ronald J. Iannotti"),
                   br("As you can see, the lonelier and sadder a student feels, the more satisfied they are with their life.
                   This, however, does not make sense, and it may just be a limitation within our selected dataset."),
                   br(),
                   br()),
                 
                 # Others
                 tabPanel("Others", fluidPage(
                   h1("What is the relationship between mental health and miscellaneous variables?"),
                   hr("Select one of the following:"),
                   radioButtons(inputId = "Other_input", label = "", choices = c("Fitness","Computer Use","Difficulty Sleeping","Teacher's Opinion on Performance")),
                   plotOutput("OtherPlot")),
                   br("Data Source: 'Health Behavior in School-Aged Children, 2009-2010' by Ronald J. Iannotti"),
                   br("As you can see, there is a positive correlation between fitness and life satisfaction. 
                      The more a student exercises, the higher their life satisfaction score.
                      Meanwhile, there is a negative correlation between computer use and life satisfaction.
                      The more a student uses the computer on the daily, the lower their life satisfaction score.
                      There is also a negative correlation between difficulty sleeping and life satisfaction.
                      The more difficulty a student experiences with sleeping, the lower their life satisfaction score.
                      Lastly, there is a positive correlation between a teacher's opinion on a student's performance
                      and their mental wellbeing. The better the teacher's opinion, the better the student's mental wellbeing.
                      And of course, these are mere correlations and not causal relationships, but it could be meaningful to see these trends."),
                   br(),
                   br()),
                 
                 inverse = TRUE, position = "static-top")
                 
     





server <- function(input, output) {
  output$MLPlot <- renderPlot({
    if (input$ML_input == "Bagging") {
      varImpPlot(bagging)
    } else if (input$ML_input == "Random Forest") {
      varImpPlot(rndForest)
    } else if (input$ML_input == "Neural Network") {
      nn_plot
    }
  })
  output$DemPlot <- renderPlot({
    if (input$Dem_input == "Gender") {
      gender_graph
    } else if (input$Dem_input == "Race") {
      race_graph
    }
  })
  output$SelfPlot <- renderPlot({
    if (input$Self_input == "Appearance Satisfaction") {
      appear_graph
    } else if (input$Self_input == "Negative Body Image") {
      body_graph
    }
  })
  output$EmoPlot <- renderPlot({
    if (input$Emo_input == "Loneliness") {
      lonely_graph
    } else if (input$Emo_input == "Sadness") {
      sad_graph
    }
  })
  output$OtherPlot <- renderPlot({
    if (input$Other_input == "Fitness") {
      phys_active
    } else if (input$Other_input == "Computer Use") {
      comp_use
    } else if (input$Other_input == "Difficulty Sleeping") {
      sleep_graph
    } else if (input$Other_input == "Teacher's Opinion on Performance") {
      teacher_graph
    }
  })
  output$legend1 <- renderText({
    "Meanings for variable names as used in this project:"
  })
  output$legend2 <- renderText({
    "Response variable:"
  })
  output$legend3 <- renderText({
    "Q7 - life satisfaction score, self-reported by respondents"
  })
  output$legend4 <- renderText({
    "Self-perception:"
  })
  output$legend5 <- renderText({
    "q8 - thoughts on body (1 - much too thin, 2 - a bit too thin, 3 - right size, 4 - a bit too fat, 5 - much too thin)"
  })
  output$legend6 <- renderText({
    "Following questions use scale 1-5:"
  })
  output$legend7 <- renderText({
    "1 = Strongly disagree"
  })
  output$legend8 <- renderText({
    "2 = Disagree"
  })
  output$legend9 <- renderText({
    "3 = Neither agree nor disagree"
  })
  output$legend10 <- renderText({
    "4 = Agree"
  })
  output$legend11 <- renderText({
    "5 = Strongly agree"
  })
  output$legend12 <- renderText({
    "38a - You are frustruated with your appearance"
  })
  output$legend13 <- renderText({
    "38b - You are satisfied with your appearance"
  })
  output$legend14 <- renderText({
    "38c - You hate your body"
  })
  output$legend15 <- renderText({
    "38d - You feel comfortable with your body"
  })
  output$legend16 <- renderText({
    "38e - You feel anger towards your body"
  })
  output$legend17 <- renderText({
    "38f - You like your appearance in spite of flaws"
  })
  output$legend18 <- renderText({
    "How often in the past 6 months did you feel..."
  })
  output$legend19 <- renderText({
    "q42d - ...low"
  })
  output$legend20 <- renderText({
    "q42e - ...irritated"
  })
  output$legend21 <- renderText({
    "q42f - ...nervous"
  })
  output$legend22 <- renderText({
    "q42g - ...trouble sleeping"
  })
  output$legend23 <- renderText({
    "How often in the past month did you take medicine for..."
  })
  output$legend24 <- renderText({
    "q43c - ...trouble sleeping"
  })
  output$legend25 <- renderText({
    "q43d - ...nervousness"
  })
  output$legend26 <- renderText({
    "How often in the past week did you..."
  })
  output$legend27<- renderText({
    "q48a - ...felt fit and well"
  })
  output$legend28 <- renderText({
    "q48b - ...felt full of energy"
  })
  output$legend29 <- renderText({
    "q48c - ...felt sad"
  })
  output$legend30 <- renderText({
    "q43d - ...felt lonely"
  })
  output$legend31 <- renderText({
    "Technology use:"
  })
  output$legend32 <- renderText({
    "q9_1 - average hours videogames played on weekdays"
  })
  output$legend33 <- renderText({
    "q9_2 - average hours videogames played on weekends"
  })
  output$legend34 <- renderText({
    "q9_comp - daily average hours of videogames played"
  })
  output$legend35 <- renderText({
    "q10_1 - average hours of computer usage on weekdays"
  })
  output$legend36 <- renderText({
    "q10_2 - average hours of computer usage on weekends"
  })
  output$legend37 <- renderText({
    "q10_comp - daily average hours of computer usage"
  })
  output$legend38 <- renderText({
    "Other:"
  })
  output$legend39 <- renderText({
    "q19 - number of days physically active during last week"
  })
  output$legend40 <- renderText({
    "q61 - teacher's opinion of school performance"
  })
  output$age <- renderText({
    "q6 - age"
  })
  output$gender <- renderText({
    "Q1 - gender"
  })
  output$extra <- renderText({
    "Descriptive varibales:"
  })
  output$home1 <- renderText({
    "We chose to study the mental health of students because we recognized the importance 
    of this topic and the impact that research has on mental health resources. Mental 
    health awareness and destigmatization is only a recent thing. According to the 
    World Psychiatric Association, mental health wasn’t talked about through a 
    medical perspective until 1946, and according to Unite for Sight, people didn’t 
    start advocating for mental health education, research, etc. until 1979.
    As you can see, mental health culture and policy has changed a lot in recent years, 
    thanks to recent research and studies."
  })
  output$dataset1 <- renderText({
    "We got our dataset from 'Health Behavior in School-Aged Children (HBSC), (2009-2010)' 
    by Ronald J. Iannotti. This dataset contains 12,642 observations of public, catholic, 
    and other private school students in grades 5 to 10 in the 50 states & the District 
    of Columbia. The survey included questions like physical activity, body image, 
    and daily computer use; and its purpose was to monitor behaviors and attitudes 
    in youths over time and gather information on early adolescent development."
  })
  output$tab1 <- renderText({
    "Demographics: gender and race variables."
  })
  output$tab2 <- renderText({
    "Self-Perception: appearance satisfaction and body image variables."
  })
  output$tab3 <- renderText({
    "Emotions: variables that measure loneliness and sadness."
  })
  output$tab4 <- renderText({
    "Machine Learning Legend: what do the machine learning methods aim to accomplish, how they work, what each variable name stands for, and how to read the output graphs."
  })
  output$tab5 <- renderText({
    "Machine Learning Graphs: variable importance graphs for the macchine learning algrithms on the data: neural network, bagging, and random forest."
  })
  output$tab6 <- renderText({
    "Other: variables such as fitness, computer use, difficulty sleeping, and teacher's opinion on performance."
  })
  output$ML_purpose <- renderText({
    "Machine learning allows data scientists to look for potential patterns in the data in order to locate trends. The point of machine learning algorithms is to
    try out different potential explanatory variables as predictive criteria for the response variable. After selecting an explanatory variable and a specific cutoff,
    observations are split into buckets by the predictive factor. After repeating this process many times, certain explanatory variables show a greater predictive effect
    for the response. The higher this predictive effect - the higher the variable importance."
  })
  output$ML_methods_nn <- renderText({
    "When a neural network is used, weights are initially assigned to each input variable at random. After going through a specified number of hidden layers,
    an activation function is applied to the modified inputs in order to achieve a predicted output. The program then calculates the size of the error, re-adjusts weights
    accordingly, and tries again, and again, and again..."
  })
  output$ML_methods_bag <- renderText({
    "The bagging method utilizes regression trees in order to evaluate variable importance. Regression trees are binary trees that split the data into two separate buckets at
    each split of the tree by some variable and a specified cutoff. When a bagging method is used, data is separated into a number of training sets, then a decision tree is
    built for each training set, and the average prediction for each observation is taken."
  })
  output$ML_methods_rf <- renderText({
    "The random forest model is similar to bagging: it also uses regression trees and a creates a separate tree for each training set. However, at each split
    of the tree, a random sample of explanatory variables is considered as potential split criteria instead of all of them. This can potentially address the issue
    of too much correlation across trees."
  })
  output$how_to_nn <- renderText({
    "Neural Network: the variable importance graph for the neural network shows two things for each explanatory variable: the strength of the relationship and whether it is positive or negative.
    The longer/taller the bar - the stronger the relationship. Positive relationships appear as bars that are positioned above the x-axis, whereas negative ones are
    represented by bars that are positioned below the x-axis."
  })
  output$how_to_bag_and_rf <- renderText({
    "Bagging and Random Forest: the variable important graphs for the bagging and random forest methods calculate variable importance in two separate ways. The left box demonstrates
    how much the mean square error of the prediction would increase if any given variable was removed from the tree models, measured in % change of the mean square error. The further a dot
    is to the right, the more important the variable. The right box demonstrates node purity, or how cleanly each variable splits the data into separate buckets, measured as the difference in
    residual sums of squares before and after a split is made on a s[ecific variable. The further a dot is to the right, the cleaner the split."
  })
}





shinyApp(ui = ui, server = server)




 